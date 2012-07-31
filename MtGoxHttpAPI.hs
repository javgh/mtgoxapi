{-# LANGUAGE OverloadedStrings #-}

module MtGoxHttpAPI
    ( getOrderCountR
    , submitBtcBuyOrder
    , submitBtcSellOrder
    , getOrderResultR
    , getWalletHistoryR
    , getPrivateInfoR
    , getBitcoinDepositAddressR
    , withdrawBitcoins
    , letOrdersExecuteR
    , submitOrder
    ) where

import Control.Applicative
import Control.Arrow
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Control.Watchdog
import Data.Aeson
import Data.Digest.Pure.SHA
import Network.Curl
import Network.HTTP.Base (urlEncodeVars)
import Text.Printf

import qualified Control.Exception as E
import qualified Data.Attoparsec as AP
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import AuthCommand
import AuthCommandReplyParsing
import CurlWrapper
import MtGoxCredentials
import StreamParsing

data HttpApiResult = HttpApiSuccess Value
                   | HttpApiFailure
                   deriving (Show)

data OrderStats = OrderStats { usdEarned :: Integer
                             , usdSpent :: Integer
                             , usdFee :: Integer
                             }
                  deriving (Show)

instance FromJSON HttpApiResult where
    parseJSON (Object o) = case H.lookup "result" o of
        Just "success" -> case H.lookup "return" o of
            Just v -> return $ HttpApiSuccess v
            Nothing -> return HttpApiFailure
        Just _ -> return HttpApiFailure
        Nothing -> return HttpApiFailure
    parseJSON _ = return HttpApiFailure

mtGoxApi :: String
mtGoxApi = "https://mtgox.com/api/"

ioTry :: IO a -> IO (Either E.IOException a)
ioTry = E.try

parseReply ::  FromJSON a => String -> Value -> a
parseReply method v =
    case fromJSON v of
        Success r -> r
        Error _ -> error ("Unexpected result when calling method " ++ method)

-- ^ Repeat an API call until it succeeds. Those functions which make use of
-- this helper are marked with the suffix 'R'.
reliableApiCall :: Maybe WatchdogLogger -> IO (Either String a) -> IO a
reliableApiCall mLogger f = watchdog $ do
    case mLogger of
        Just logger -> setLoggingAction logger
        Nothing -> return ()
    watch f

callApi :: CurlChan-> MtGoxCredentials-> URLString-> [(String, String)]-> IO (Either String HttpApiResult)
callApi curlChan mtGoxCred uri parameters = do
    nonce <- getNonce
    let parameters' = ("nonce", T.unpack nonce) : parameters
        (headers, body) = compileRequest mtGoxCred parameters'
    (status, payload) <- performCurlRequest curlChan uri
                            [ CurlHttpHeaders headers
                            , CurlPostFields [body]
                            ]
    return $ case status of
        CurlOK -> case AP.parseOnly json (B8.pack payload) of
            Left err' -> Left $ "JSON parse error: " ++ err'
            Right json -> case fromJSON json of
                (Error err'') -> Left $ "API parse error: " ++ err''
                (Success v) -> Right v :: Either String HttpApiResult
        err -> Left $ "Curl error: " ++ show err

compileRequest :: MtGoxCredentials -> [(String, String)] -> ([String], String)
compileRequest credentials parameters =
    let authSecretDecoded = mgcAuthSecretDecoded credentials
        authKey = mgcAuthKey credentials
        body = urlEncodeVars parameters
        hmac = hmacSha512 (BL.fromChunks [authSecretDecoded]) (BL8.pack body)
        hmacFormatted = B64.encode . foldl1 B.append
                            . BL.toChunks . bytestringDigest $ hmac
        headers = [ "Rest-Key: " ++ B8.unpack authKey
                  , "Rest-Sign: " ++ B8.unpack hmacFormatted
                  ]
    in (headers, body)

getOrderCountR :: Maybe WatchdogLogger -> CurlChan -> MtGoxCredentials -> IO (Maybe OpenOrderCountReply)
getOrderCountR mLogger curlChan mtGoxCreds = do
    let uri = mtGoxApi ++ "1/generic/private/orders"
    v <- reliableApiCall mLogger $ callApi curlChan mtGoxCreds uri []
    return $ case v of
        HttpApiFailure -> Nothing
        HttpApiSuccess v' -> 
            Just (parseReply "getOrderCountR" v') :: Maybe OpenOrderCountReply

submitBtcBuyOrder :: CurlChan -> MtGoxCredentials -> Integer -> IO (Either String OrderReply)
submitBtcBuyOrder curlChan mtGoxCreds amount = do
    let uri = mtGoxApi ++ "1/BTCUSD/private/order/add"
        parameters = [ ("type", "bid")
                     , ("amount_int", show amount)
                     , ("prefer_fiat_fee", "1")
                     ]
    v <- callApi curlChan mtGoxCreds uri parameters
    return $ case v of
        Left err -> Left err
        Right HttpApiFailure -> Left "HttpApiFailure when doing submitBtcBuyOrder"
        Right (HttpApiSuccess v') ->
            Right (parseReply "submitBtcBuyOrder" v') :: Either String OrderReply

submitBtcSellOrder :: CurlChan -> MtGoxCredentials -> Integer -> IO (Either String OrderReply)
submitBtcSellOrder curlChan mtGoxCreds amount = do
    let uri = mtGoxApi ++ "1/BTCUSD/private/order/add"
        parameters = [ ("type", "ask")
                     , ("amount_int", show amount)
                     , ("prefer_fiat_fee", "1")
                     ]
    v <- callApi curlChan mtGoxCreds uri parameters
    return $ case v of
        Left err -> Left err
        Right HttpApiFailure -> Left "HttpApiFailure when doing submitBtcBuyOrder"
        Right (HttpApiSuccess v') ->
            Right (parseReply "submitBtcSellOrder" v') :: Either String OrderReply

getOrderResultR :: Maybe WatchdogLogger-> CurlChan-> MtGoxCredentials-> OrderType-> OrderID-> IO (Maybe OrderResult)
getOrderResultR mLogger curlChan mtGoxCreds orderType orderID = do
    let uri = mtGoxApi ++ "1/generic/private/order/result"
        parameters = [ ("type", case orderType of
                                    OrderTypeBuyBTC -> "bid"
                                    OrderTypeSellBTC -> "ask")
                     , ("order", T.unpack (oid orderID))
                     ]
    v <- reliableApiCall mLogger $ callApi curlChan mtGoxCreds uri parameters
    return $ case v of
        HttpApiFailure -> Nothing
        HttpApiSuccess v' -> 
            Just (parseReply "getOrderResultR" v') :: Maybe OrderResult

getWalletHistoryR :: Maybe WatchdogLogger-> CurlChan-> MtGoxCredentials-> TradeID-> IO (Maybe WalletHistory)
getWalletHistoryR mLogger curlChan mtGoxCreds tradeID = do
    let uri = mtGoxApi ++ "1/generic/private/wallet/history"
        parameters = [ ("currency", "USD")
                     , ("trade_id", T.unpack (tid tradeID))
                     ]
    v <- reliableApiCall mLogger $ callApi curlChan mtGoxCreds uri parameters
    return $ case v of
        HttpApiFailure -> Nothing
        HttpApiSuccess v' -> 
            Just (parseReply "getWalletHistoryR" v') :: Maybe WalletHistory

getPrivateInfoR :: Maybe WatchdogLogger-> CurlChan -> MtGoxCredentials -> IO (Maybe PrivateInfoReply)
getPrivateInfoR mLogger curlChan mtGoxCreds = do
    let uri = mtGoxApi ++ "1/generic/private/info"
    v <- reliableApiCall mLogger $ callApi curlChan mtGoxCreds uri []
    return $ case v of
        HttpApiFailure -> Nothing
        HttpApiSuccess v' -> 
            Just (parseReply "getPrivateInfoR" v') :: Maybe PrivateInfoReply

getBitcoinDepositAddressR mLogger curlChan mtGoxCreds = do
    let uri = mtGoxApi ++ "1/generic/bitcoin/address"
    v <- reliableApiCall mLogger $ callApi curlChan mtGoxCreds uri []
    return $ case v of
        HttpApiFailure -> Nothing
        HttpApiSuccess v' -> 
            Just (parseReply "getBitcoinDepositAddressR" v') :: Maybe BitcoinDepositAddress

withdrawBitcoins :: CurlChan-> MtGoxCredentials-> BitcoinAddress-> Integer-> IO (Either String WithdrawStatus)
withdrawBitcoins curlChan mtGoxCreds (BitcoinAddress addr) amount = do
    let factor = 10 ^ (8 :: Integer)
        doubleAmount = fromIntegral amount / factor
        uri = mtGoxApi ++ "0/withdraw.php"
        parameters = [ ("group1", "BTC")
                     , ("btca", T.unpack addr)
                     , ("amount", printf "%.8f" (doubleAmount :: Double))
                     ]
    customCallApi uri parameters
  where
    customCallApi uri parameters = do   -- needed for accessing version 0 API,
                                        -- as this functionality is not (yet?)
                                        -- available in version 1
        nonce <- getNonce
        let parameters' = ("nonce", T.unpack nonce) : parameters
            (headers, body) = compileRequest mtGoxCreds parameters'
        (status, payload) <- performCurlRequest curlChan uri
                                [ CurlHttpHeaders headers
                                , CurlPostFields [body]
                                ]
        return $ case status of
            CurlOK -> case AP.parseOnly json (B8.pack payload) of
                Left err' ->
                    Left $ "JSON parse error when calling old API: " ++ err'
                Right json -> case fromJSON json of
                    (Error err'') ->
                        Left $ "API parse error when calling old API: " ++ err''
                    (Success v) -> Right v :: Either String WithdrawStatus
            err -> Left $ "Curl error: " ++ show err

letOrdersExecuteR :: Maybe WatchdogLogger-> CurlChan -> MtGoxCredentials -> IO (Maybe OpenOrderCountReply)
letOrdersExecuteR mLogger curlChan mtGoxCreds =
    watchdog $ do
        setInitialDelay 200000 {- 200 ms -}
        setMaximumDelay 100000000 {- 10 seconds -}
        setLoggingAction silentLogger {- no logging -}
        watch task
  where
    task = do
        orderCount <- getOrderCountR mLogger curlChan mtGoxCreds
        case orderCount of
            Nothing -> return $ Right Nothing
            Just (OpenOrderCountReply count) ->
                if count > 0
                    then return $ Left "still outstanding orders"
                    else return $ Right orderCount

processWalletHistories ::  [WalletHistory] -> OrderStats
processWalletHistories histories =
    let entries = concatMap whEntries histories
        amounts = map (weType &&& weAmount) entries
        usdEarned = filter ((USDEarned ==) . fst) amounts
        usdSpent = filter ((USDSpent ==) . fst) amounts
        usdFee = filter ((USDFee ==) . fst) amounts
    in OrderStats { usdEarned = sum (map snd usdEarned)
                  , usdSpent = sum (map snd usdSpent)
                  , usdFee = sum (map snd usdFee)
                  }

-- ^ Submit an order and return 'OrderStats'. In case of some non-critical
-- errors things are re-tried automatically, but if API errors happen or network
-- errors occur during critical phases (like placing the order) a 'Left' with
-- the error is returned.
submitOrder :: Maybe WatchdogLogger-> CurlChan-> MtGoxCredentials-> OrderType-> Integer-> IO (Either String OrderStats)
submitOrder mLogger curlChan mtGoxCreds orderType amount = runEitherT $ do
    -- step 1: make sure network connection is present
    --         and no orders are pending
    step1 <- liftIO $ letOrdersExecuteR mLogger curlChan mtGoxCreds
    (OpenOrderCountReply _) <- liftEither (note "letOrdersExecute failed" step1)
    -- step 2: submit order
    orderReply <-
        liftIO (case orderType of
            OrderTypeBuyBTC -> submitBtcBuyOrder curlChan mtGoxCreds amount
            OrderTypeSellBTC -> submitBtcSellOrder curlChan mtGoxCreds amount)
        >>= liftEither
    -- step 3: wait for order to complete
    step3 <- liftIO $ letOrdersExecuteR mLogger curlChan mtGoxCreds
    (OpenOrderCountReply _) <-
        liftEither (note "after submitting order letOrdersExecute failed" step3)
    -- step 4: get trade ids
    let orderID = orOrderID orderReply
    step4 <-
        liftIO $ getOrderResultR mLogger curlChan mtGoxCreds orderType orderID
    orderResult <- liftEither (note "getOrderResultR failed" step4)
    let tradeIDs = orTradeIDs orderResult
    -- step 5: collect wallet entries for all trade ids
    step5 <- forM tradeIDs $ \tradeID ->
                liftIO (getWalletHistory tradeID) >>= liftEither
    return $ processWalletHistories step5
  where
    getWalletHistory tradeID = do
        history <- getWalletHistoryR mLogger curlChan mtGoxCreds tradeID
        return $ note "getWalletHistoryR failed" history

--debug = do
--    c <- initCurlWrapper
    -- 040982ab-04d9-4f2a-8455-316a47e75389
    --getOrderResultR Nothing c debugCredentials OrderTypeSellBTC "040982ab-04d9-4f2a-8455-316a47e75389" >>= print
    --getWalletHistoryR Nothing c debugCredentials (TradeID "1343679373020988") >>= print
    --letOrdersExecuteR Nothing c debugCredentials >>= print
    --submitOrder Nothing c debugCredentials OrderTypeSellBTC 1000000
    --submitOrder Nothing c debugCredentials OrderTypeSellBTC 1000000
    --getBitcoinDepositAddressR Nothing c debugCredentials
    --let addr = BitcoinAddress "1AvcR6BFmnd8SnZiJfDm6rrQ2aTkdHsf6N"
    --withdrawBitcoins c debugCredentials addr 1000000 >>= print
