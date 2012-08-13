{-# LANGUAGE OverloadedStrings #-}

module Network.MtGoxAPI.HttpAPI
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
    , OrderStats(..)
    ) where

import Control.Arrow
import Control.Error
import Control.Monad
import Control.Watchdog
import Data.Aeson
import Data.Digest.Pure.SHA
import Network.Curl
import Network.HTTP.Base (urlEncodeVars)
import Text.Printf

import qualified Data.Attoparsec as AP
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Network.MtGoxAPI.Credentials
import Network.MtGoxAPI.CurlWrapper
import Network.MtGoxAPI.StreamAuthCommands
import Network.MtGoxAPI.Types

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

callApi :: CurlHandle -> MtGoxCredentials-> URLString-> [(String, String)]-> IO (Either String HttpApiResult)
callApi curlHandle mtGoxCred uri parameters = do
    nonce <- getNonce
    let parameters' = ("nonce", T.unpack nonce) : parameters
        (headers, body) = compileRequest mtGoxCred parameters'
    (status, payload) <- performCurlRequest curlHandle uri
                            [ CurlHttpHeaders headers
                            , CurlPostFields [body]
                            ]
    return $ case status of
        CurlOK -> case AP.parseOnly json (B8.pack payload) of
            Left err' -> Left $ "JSON parse error: " ++ err'
            Right jsonV -> case fromJSON jsonV of
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

getOrderCountR :: Maybe WatchdogLogger -> CurlHandle  -> MtGoxCredentials -> IO (Maybe OpenOrderCount)
getOrderCountR mLogger curlHandle mtGoxCreds = do
    let uri = mtGoxApi ++ "1/generic/private/orders"
    v <- reliableApiCall mLogger $ callApi curlHandle mtGoxCreds uri []
    return $ case v of
        HttpApiFailure -> Nothing
        HttpApiSuccess v' -> 
            Just (parseReply "getOrderCountR" v') :: Maybe OpenOrderCount

submitBtcBuyOrder :: CurlHandle  -> MtGoxCredentials -> Integer -> IO (Either String Order)
submitBtcBuyOrder curlHandle mtGoxCreds amount = do
    let uri = mtGoxApi ++ "1/BTCUSD/private/order/add"
        parameters = [ ("type", "bid")
                     , ("amount_int", show amount)
                     , ("prefer_fiat_fee", "1")
                     ]
    v <- callApi curlHandle mtGoxCreds uri parameters
    return $ case v of
        Left err -> Left err
        Right HttpApiFailure -> Left "HttpApiFailure when doing submitBtcBuyOrder"
        Right (HttpApiSuccess v') ->
            Right (parseReply "submitBtcBuyOrder" v') :: Either String Order

submitBtcSellOrder :: CurlHandle  -> MtGoxCredentials -> Integer -> IO (Either String Order)
submitBtcSellOrder curlHandle mtGoxCreds amount = do
    let uri = mtGoxApi ++ "1/BTCUSD/private/order/add"
        parameters = [ ("type", "ask")
                     , ("amount_int", show amount)
                     , ("prefer_fiat_fee", "1")
                     ]
    v <- callApi curlHandle mtGoxCreds uri parameters
    return $ case v of
        Left err -> Left err
        Right HttpApiFailure -> Left "HttpApiFailure when doing submitBtcBuyOrder"
        Right (HttpApiSuccess v') ->
            Right (parseReply "submitBtcSellOrder" v') :: Either String Order

getOrderResultR :: Maybe WatchdogLogger-> CurlHandle -> MtGoxCredentials-> OrderType-> OrderID-> IO (Maybe OrderResult)
getOrderResultR mLogger curlHandle mtGoxCreds orderType orderID = do
    let uri = mtGoxApi ++ "1/generic/private/order/result"
        parameters = [ ("type", case orderType of
                                    OrderTypeBuyBTC -> "bid"
                                    OrderTypeSellBTC -> "ask")
                     , ("order", T.unpack (oid orderID))
                     ]
    v <- reliableApiCall mLogger $ callApi curlHandle mtGoxCreds uri parameters
    return $ case v of
        HttpApiFailure -> Nothing
        HttpApiSuccess v' -> 
            Just (parseReply "getOrderResultR" v') :: Maybe OrderResult

getWalletHistoryR :: Maybe WatchdogLogger-> CurlHandle -> MtGoxCredentials-> TradeID-> IO (Maybe WalletHistory)
getWalletHistoryR mLogger curlHandle mtGoxCreds tradeID = do
    let uri = mtGoxApi ++ "1/generic/private/wallet/history"
        parameters = [ ("currency", "USD")
                     , ("trade_id", T.unpack (tid tradeID))
                     ]
    v <- reliableApiCall mLogger $ callApi curlHandle mtGoxCreds uri parameters
    return $ case v of
        HttpApiFailure -> Nothing
        HttpApiSuccess v' -> 
            Just (parseReply "getWalletHistoryR" v') :: Maybe WalletHistory

getPrivateInfoR :: Maybe WatchdogLogger-> CurlHandle  -> MtGoxCredentials -> IO (Maybe PrivateInfo)
getPrivateInfoR mLogger curlHandle mtGoxCreds = do
    let uri = mtGoxApi ++ "1/generic/private/info"
    v <- reliableApiCall mLogger $ callApi curlHandle mtGoxCreds uri []
    return $ case v of
        HttpApiFailure -> Nothing
        HttpApiSuccess v' -> 
            Just (parseReply "getPrivateInfoR" v') :: Maybe PrivateInfo

getBitcoinDepositAddressR :: Maybe WatchdogLogger-> CurlHandle-> MtGoxCredentials-> IO (Maybe BitcoinDepositAddress)
getBitcoinDepositAddressR mLogger curlHandle mtGoxCreds = do
    let uri = mtGoxApi ++ "1/generic/bitcoin/address"
    v <- reliableApiCall mLogger $ callApi curlHandle mtGoxCreds uri []
    return $ case v of
        HttpApiFailure -> Nothing
        HttpApiSuccess v' -> 
            Just (parseReply "getBitcoinDepositAddressR" v') :: Maybe BitcoinDepositAddress

withdrawBitcoins :: CurlHandle -> MtGoxCredentials-> BitcoinAddress-> Integer-> IO (Either String WithdrawStatus)
withdrawBitcoins curlHandle mtGoxCreds (BitcoinAddress addr) amount = do
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
        (status, payload) <- performCurlRequest curlHandle uri
                                [ CurlHttpHeaders headers
                                , CurlPostFields [body]
                                ]
        return $ case status of
            CurlOK -> case AP.parseOnly json (B8.pack payload) of
                Left err' ->
                    Left $ "JSON parse error when calling old API: " ++ err'
                Right jsonV -> case fromJSON jsonV of
                    (Error err'') ->
                        Left $ "API parse error when calling old API: " ++ err''
                    (Success v) -> Right v :: Either String WithdrawStatus
            err -> Left $ "Curl error: " ++ show err

letOrdersExecuteR :: Maybe WatchdogLogger-> CurlHandle  -> MtGoxCredentials -> IO (Maybe OpenOrderCount)
letOrdersExecuteR mLogger curlHandle mtGoxCreds =
    watchdog $ do
        setInitialDelay 200000 {- 200 ms -}
        setMaximumDelay 100000000 {- 10 seconds -}
        setLoggingAction silentLogger {- no logging -}
        watch task
  where
    task = do
        orderCount <- getOrderCountR mLogger curlHandle mtGoxCreds
        case orderCount of
            Nothing -> return $ Right Nothing
            Just (OpenOrderCount count) ->
                if count > 0
                    then return $ Left "still outstanding orders"
                    else return $ Right orderCount

processWalletHistories ::  [WalletHistory] -> OrderStats
processWalletHistories histories =
    let entries = concatMap whEntries histories
        amounts = map (weType &&& weAmount) entries
        usdEarnedL = filter ((USDEarned ==) . fst) amounts
        usdSpentL = filter ((USDSpent ==) . fst) amounts
        usdFeeL = filter ((USDFee ==) . fst) amounts
    in OrderStats { usdEarned = sum (map snd usdEarnedL)
                  , usdSpent = sum (map snd usdSpentL)
                  , usdFee = sum (map snd usdFeeL)
                  }

-- ^ Submit an order and return 'OrderStats'. In case of some non-critical
-- errors things are re-tried automatically, but if API errors happen or network
-- errors occur during critical phases (like placing the order) a 'Left' with
-- the error is returned.
submitOrder :: Maybe WatchdogLogger-> CurlHandle -> MtGoxCredentials-> OrderType-> Integer-> IO (Either String OrderStats)
submitOrder mLogger curlHandle mtGoxCreds orderType amount = runEitherT $ do
    -- step 1: make sure network connection is present
    --         and no orders are pending
    (OpenOrderCount _) <- noteT "letOrdersExecute failed" . MaybeT $
                            letOrdersExecuteR mLogger curlHandle mtGoxCreds
    -- step 2: submit order
    order <- EitherT $ case orderType of
                        OrderTypeBuyBTC ->
                            submitBtcBuyOrder curlHandle mtGoxCreds amount
                        OrderTypeSellBTC ->
                            submitBtcSellOrder curlHandle mtGoxCreds amount
    -- step 3: wait for order to complete
    (OpenOrderCount _) <-
        noteT "after submitting order letOrdersExecute failed" . MaybeT $
            letOrdersExecuteR mLogger curlHandle mtGoxCreds
    -- step 4: get trade ids
    let orderID = oOrderID order
    orderResult <-
        noteT "getOrderResultR failed" . MaybeT $
            getOrderResultR mLogger curlHandle mtGoxCreds orderType orderID
    let tradeIDs = orTradeIDs orderResult
    -- step 5: collect wallet entries for all trade ids
    histories <- forM tradeIDs $ \tradeID -> EitherT (getWalletHistory tradeID)
    return $ processWalletHistories histories
  where
    getWalletHistory tradeID = do
        history <- getWalletHistoryR mLogger curlHandle mtGoxCreds tradeID
        return $ note "getWalletHistoryR failed" history
