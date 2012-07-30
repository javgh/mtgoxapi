{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Control.Watchdog
import Data.Aeson
import Data.Aeson.Types
import Data.Digest.Pure.SHA
import Data.Hashable
import Data.List
import Data.Maybe
import Data.String
import Data.Time.Clock.POSIX
import Data.Word (Word8)
import Network.Curl
import Network.HTTP.Base (urlEncodeVars)

import qualified Codec.Binary.Base64 as Base64
import qualified Data.Attoparsec as AP
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

type Octet = Word8

data MtGoxFunds = MtGoxFunds { mgfBtcBalance :: Integer
                             , mgfUsdBalance :: Integer
                             }
                             deriving (Show)

data MtGoxOrders = MtGoxOrders Integer
                   deriving (Show)

data MtGoxOrderQueued = MtGoxOrderQueued
                        deriving (Show)

instance FromJSON MtGoxFunds
  where
    parseJSON (Object o) = case extractBalances o of
        Just (btcV, usdV) -> do
            btcS <- parseJSON btcV :: Parser String
            usdS <- parseJSON usdV :: Parser String
            return $ MtGoxFunds (read btcS) (read usdS)
        Nothing -> mzero
    parseJSON _ = mzero

instance FromJSON MtGoxOrders
  where
    parseJSON (Object o) = case H.lookup "return" o of
        Just (Array orders) ->
            return $ MtGoxOrders (fromIntegral . V.length $ orders)
        Just _ -> mzero
        Nothing -> mzero
    parseJSON _ = mzero

instance FromJSON MtGoxOrderQueued
  where
    parseJSON (Object o) = case H.lookup "status" o of
        Just statusV -> do
            status <- parseJSON statusV :: Parser String
            if "queued" `isInfixOf` status
                then return MtGoxOrderQueued
                else mzero
        Nothing -> mzero
    parseJSON _ = mzero

hardcodedAuthKey :: String
hardcodedAuthKey = "5be9c2b8-6f85-408d-a8de-18db2f5fc3af"

hardcodedAuthSecret :: String
hardcodedAuthSecret = "A0seCE45EgkyUOliU3wokMuRwq/AbdpVMJ353kmDXtH0mWdy9QqsSKVkAer1AbfG4bYUO4YKVyh2ovO4pR1lag=="

mtgoxApi :: String
mtgoxApi = "https://mtgox.com/api/"

extractBalances :: (Eq k, IsString k, Hashable k) =>H.HashMap k Value -> Maybe (Value, Value)
extractBalances o = do
    btc <- extractBalance "BTC" o
    usd <- extractBalance "USD" o
    return (btc, usd)

extractBalance :: (Eq k, IsString k, Hashable k) =>T.Text -> H.HashMap k Value -> Maybe Value
extractBalance currency o = do
    balance <- H.lookup "return" o
                >>= extractObject
                >>= H.lookup "Wallets"
                >>= extractObject
                >>= H.lookup currency
                >>= extractObject
                >>= H.lookup "Balance"
                >>= extractObject
    H.lookup "value_int" balance

extractObject :: Value -> Maybe Object
extractObject (Object o) = Just o
extractObject _ = Nothing

getNonce :: IO Integer
getNonce = do
    now <- getPOSIXTime
    return $ round (now * 1000000)

stringToOctets :: String -> [Octet]
stringToOctets = map (fromIntegral . fromEnum)

octetsToString :: [Octet] -> String
octetsToString = map (toEnum . fromIntegral)

base64encode :: String -> String
base64encode = Base64.encode . stringToOctets

base64decode :: String -> Maybe String
base64decode = fmap octetsToString . Base64.decode

compileHeaders :: String -> String -> String -> [String]
compileHeaders authKey authSecret body =
    let authSecretDecoded = fromMaybe
                                (error "authSecret needs to be encoded in base 64")
                                (base64decode authSecret)
        hmac = hmacSha512 (BL8.pack authSecretDecoded) (BL8.pack body)
        hmacFormatted = base64encode . BL8.unpack . bytestringDigest $ hmac
    in ["Rest-Key: " ++ authKey, "Rest-Sign: " ++ hmacFormatted]

performRequest :: String -> String -> URLString-> [(String, String)]-> IO (Maybe String)
performRequest authKey authSecret uri parameters = do
    nonce <- getNonce
    let parameters' = ("nonce", show nonce) : parameters
        body = urlEncodeVars parameters'
        headers = compileHeaders authKey authSecret body
    (status, payload) <- curlGetString uri [ CurlVerbose False
                          , CurlUserAgent "libcurl"
                          , CurlHttpHeaders headers
                          , CurlPostFields [body]
                          ]
    case status of
        CurlOK -> return $ Just payload
        _ -> return Nothing

collapseErrors :: Either String (Result b) -> Either String b
collapseErrors (Left err) = Left err
collapseErrors (Right (Error err)) = Left err
collapseErrors (Right (Success payload)) = Right payload

parsePayload :: (FromJSON b) => String -> Either String b
parsePayload = collapseErrors . AP.parseOnly (fromJSON <$> json) . B8.pack

getFunds :: IO (Maybe MtGoxFunds)
getFunds = do
    request <- performRequest hardcodedAuthKey hardcodedAuthSecret
                (mtgoxApi ++ "1/generic/private/info") []
    case request of
        Just payload -> case parsePayload payload :: Either String MtGoxFunds  of
            Left _ -> return Nothing
            Right funds -> return $ Just funds
        Nothing -> return Nothing

getOrders :: IO (Maybe MtGoxOrders)
getOrders = do
    request <- performRequest hardcodedAuthKey hardcodedAuthSecret
                (mtgoxApi ++ "1/generic/private/orders") []
    case request of
        Just payload -> case parsePayload payload :: Either String MtGoxOrders of
            Left _ -> return Nothing
            Right orders -> return $ Just orders
        Nothing -> return Nothing

-- TODO: ueberlegen, wie man sicher geht, dass Gebote nicht
--       mehrmals eingestellt werden, wenn Fehler auftreten

placeOrder :: String -> String -> IO (Maybe MtGoxOrderQueued)
placeOrder action amount = do
    request <- performRequest hardcodedAuthKey hardcodedAuthSecret
                (mtgoxApi ++ "0/" ++ action ++ "BTC.php") [("amount", amount), ("Currency", "USD")]
    case request of
        Just payload -> case parsePayload payload :: Either String MtGoxOrderQueued of
            Left _ -> return Nothing
            Right confirmation -> return $ Just confirmation
        Nothing -> return Nothing

debugRequest :: IO()
debugRequest = do
    request <- performRequest hardcodedAuthKey hardcodedAuthSecret
                --(mtgoxApi ++ "1/BTCUSD/private/order/add") [("type", "bid"), ("amount_int", "1000000")]
                --(mtgoxApi ++ "1/BTCUSD/private/order/add") [("type", "bid"), ("amount_int", "1000000"), ("price_int", "1")]
                (mtgoxApi ++ "1/USDBTC/private/order/add") [("type", "bid"), ("amount_int", "100")]
    print request

debugRequest2 :: IO()
debugRequest2 = do
    request <- performRequest hardcodedAuthKey hardcodedAuthSecret
                (mtgoxApi ++ "1/generic/private/order/result") [("type", "bid"), ("order", "8d86b559-9551-43ef-95dd-d6c6eb0575bb")]
    print request

debugRequest3 :: IO()
debugRequest3 = do
    request <- performRequest hardcodedAuthKey hardcodedAuthSecret
                (mtgoxApi ++ "1/generic/private/trades") []
    print request

debugRequest4 :: IO()
debugRequest4 = do
    request <- performRequest hardcodedAuthKey hardcodedAuthSecret
                (mtgoxApi ++ "1/generic/private/wallet/history") [("trade_id", "1337342838083902")]
    print request

letOrdersExecute :: IO (Maybe MtGoxOrders)
letOrdersExecute =
    watchdog $ do
        setInitialDelay 100000 {- 100 ms -}
        setMaximumDelay 100000000 {- 10 seconds -}
        setLoggingAction silentLogger {- no logging -}
        watch task
  where
    task = do
        orders <- getOrders
        case orders of
            Nothing -> return $ Right Nothing
            Just (MtGoxOrders count) ->
                if count > 0
                    then return $ Left "still outstanding orders"
                    else return $ Right orders

main :: IO ()
main = do
    --debugRequest
    getFunds >>= print
    --debugRequest
    ----placeOrder "sell" "0.01" >>= print
    ----placeOrder "buy" "0.01" >>= print
    --_ <- letOrdersExecute
    --getFunds >>= print
    return ()
