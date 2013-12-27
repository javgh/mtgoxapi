-- |
-- Functions that are marked with the suffix 'R' retry automatically in case of
-- failure up to a certain number of times. However, they will return after
-- about 20 seconds in the worst case. Exceptions: 'letOrdersExecuteR' and
-- 'submitOrder'.

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

import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Control.Watchdog
import Data.Aeson
import Data.Digest.Pure.SHA
import Network.Curl
import Network.HTTP.Base (urlEncodeVars)

import qualified Control.Arrow as A
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

watchdogSettings :: WatchdogAction ()
watchdogSettings = do
    setInitialDelay 250000    -- 250 ms
    setMaximumRetries 6
    -- will fail after:
    -- 0.25 + 0.5 + 1 + 2 + 4 + 8 seconds = 15.75 seconds

parseReply ::  FromJSON a => String -> Value -> a
parseReply method v =
    case fromJSON v of
        Success r -> r
        Error _ -> error ("Unexpected result when calling method " ++ method)

robustApiCall :: Maybe WatchdogLogger-> IO (Either String b) -> IO (Either String b)
robustApiCall mLogger f = watchdog $ do
    watchdogSettings
    case mLogger of
        Just logger -> setLoggingAction logger
        Nothing -> return ()
    watchImpatiently f

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
        errMsg -> Left $ "Curl error: " ++ show errMsg

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

getOrderCountR :: Maybe WatchdogLogger -> CurlHandle  -> MtGoxCredentials -> IO (Either String OpenOrderCount)
getOrderCountR mLogger curlHandle mtGoxCreds = do
    let uri = mtGoxApi ++ "1/generic/private/orders"
    v <- robustApiCall mLogger $ callApi curlHandle mtGoxCreds uri []
    return $ case v of
        Left errMsg -> Left errMsg
        Right HttpApiFailure -> Left "HttpApiFailure when doing getOrderCountR"
        Right (HttpApiSuccess v') ->
            Right (parseReply "getOrderCountR" v') :: Either String OpenOrderCount

submitBtcBuyOrder :: CurlHandle  -> MtGoxCredentials -> Integer -> IO (Either String Order)
submitBtcBuyOrder curlHandle mtGoxCreds amount = do
    let uri = mtGoxApi ++ "1/BTCEUR/private/order/add"
        parameters = [ ("type", "bid")
                     , ("amount_int", show amount)
                     , ("prefer_fiat_fee", "1")
                     ]
    v <- callApi curlHandle mtGoxCreds uri parameters
    return $ case v of
        Left errMsg -> Left errMsg
        Right HttpApiFailure -> Left "HttpApiFailure when doing submitBtcBuyOrder"
        Right (HttpApiSuccess v') ->
            Right (parseReply "submitBtcBuyOrder" v') :: Either String Order

submitBtcSellOrder :: CurlHandle  -> MtGoxCredentials -> Integer -> IO (Either String Order)
submitBtcSellOrder curlHandle mtGoxCreds amount = do
    let uri = mtGoxApi ++ "1/BTCEUR/private/order/add"
        parameters = [ ("type", "ask")
                     , ("amount_int", show amount)
                     , ("prefer_fiat_fee", "1")
                     ]
    v <- callApi curlHandle mtGoxCreds uri parameters
    return $ case v of
        Left errMsg -> Left errMsg
        Right HttpApiFailure -> Left "HttpApiFailure when doing submitBtcSellOrder"
        Right (HttpApiSuccess v') ->
            Right (parseReply "submitBtcSellOrder" v') :: Either String Order

getOrderResultR :: Maybe WatchdogLogger-> CurlHandle -> MtGoxCredentials-> OrderType-> OrderID-> IO (Either String OrderResult)
getOrderResultR mLogger curlHandle mtGoxCreds orderType orderID = do
    let uri = mtGoxApi ++ "1/generic/private/order/result"
        parameters = [ ("type", case orderType of
                                    OrderTypeBuyBTC -> "bid"
                                    OrderTypeSellBTC -> "ask")
                     , ("order", T.unpack (oid orderID))
                     ]
    v <- robustApiCall mLogger $ callApi curlHandle mtGoxCreds uri parameters
    return $ case v of
        Left errMsg -> Left errMsg
        Right HttpApiFailure -> Left "HttpApiFailure when doing getOrderResultR"
        Right (HttpApiSuccess v') ->
            Right (parseReply "getOrderResultR" v') :: Either String OrderResult

getWalletHistoryR :: Maybe WatchdogLogger-> CurlHandle -> MtGoxCredentials-> TradeID-> IO (Either String WalletHistory)
getWalletHistoryR mLogger curlHandle mtGoxCreds tradeID = do
    let uri = mtGoxApi ++ "1/generic/private/wallet/history"
        parameters = [ ("currency", "EUR")
                     , ("trade_id", T.unpack (tid tradeID))
                     ]
    v <- robustApiCall mLogger $ callApi curlHandle mtGoxCreds uri parameters
    return $ case v of
        Left errMsg -> Left errMsg
        Right HttpApiFailure -> Left "HttpApiFailure when doing getWalletHistoryR"
        Right (HttpApiSuccess v') ->
            Right (parseReply "getWalletHistoryR" v') :: Either String WalletHistory

getPrivateInfoR :: Maybe WatchdogLogger-> CurlHandle  -> MtGoxCredentials -> IO (Either String PrivateInfo)
getPrivateInfoR mLogger curlHandle mtGoxCreds = do
    let uri = mtGoxApi ++ "1/generic/private/info"
    v <- robustApiCall mLogger $ callApi curlHandle mtGoxCreds uri []
    return $ case v of
        Left errMsg -> Left errMsg
        Right HttpApiFailure -> Left "HttpApiFailure when doing getPrivateInfoR"
        Right (HttpApiSuccess v') -> 
            Right (parseReply "getPrivateInfoR" v') :: Either String PrivateInfo

getBitcoinDepositAddressR :: Maybe WatchdogLogger-> CurlHandle-> MtGoxCredentials-> IO (Either String BitcoinDepositAddress)
getBitcoinDepositAddressR mLogger curlHandle mtGoxCreds = do
    let uri = mtGoxApi ++ "1/generic/bitcoin/address"
    v <- robustApiCall mLogger $ callApi curlHandle mtGoxCreds uri []
    return $ case v of
        Left errMsg -> Left errMsg
        Right HttpApiFailure -> Left "HttpApiFailure when doing getBitcoinDepositAddressR"
        Right (HttpApiSuccess v') ->
            Right (parseReply "getBitcoinDepositAddressR" v') :: Either String BitcoinDepositAddress

withdrawBitcoins :: CurlHandle -> MtGoxCredentials-> BitcoinAddress-> Integer-> IO (Either String WithdrawResult)
withdrawBitcoins curlHandle mtGoxCreds (BitcoinAddress addr) amount = do
    let uri = mtGoxApi ++ "1/generic/bitcoin/send_simple"
        parameters = [ ("address", T.unpack addr)
                     , ("amount_int", show amount)
                     ]
    v <- callApi curlHandle mtGoxCreds uri parameters
    return $ case v of
        Left errMsg -> Left errMsg
        Right HttpApiFailure -> Left "HttpApiFailure when doing withdrawBitcoins"
        Right (HttpApiSuccess v') ->
            Right (parseReply "withdrawBitcoins" v') :: Either String WithdrawResult

-- | Will not return until all orders have been executed. It will give up after
-- about 3 minutes, if there are persistent errors or still open orders.
letOrdersExecuteR :: Maybe WatchdogLogger-> CurlHandle-> MtGoxCredentials-> IO (Either String ())
letOrdersExecuteR mLogger curlHandle mtGoxCreds =
    watchdog $ do
       watchdogSettings
       setLoggingAction silentLogger {- no logging -}
       watchImpatiently task
  where
    task = do
        orderCount <- getOrderCountR mLogger curlHandle mtGoxCreds
        return $ case orderCount of
                    Left errMsg -> Left errMsg
                    Right (OpenOrderCount count) ->
                        if count > 0
                            then Left "still outstanding orders"
                            else Right ()

processWalletHistories ::  [WalletHistory] -> OrderStats
processWalletHistories histories =
    let entries = concatMap whEntries histories
        amounts = map (weType A.&&& weAmount) entries
        usdEarnedL = filter ((USDEarned ==) . fst) amounts
        usdSpentL = filter ((USDSpent ==) . fst) amounts
        usdFeeL = filter ((USDFee ==) . fst) amounts
    in OrderStats { usdEarned = sum (map snd usdEarnedL)
                  , usdSpent = sum (map snd usdSpentL)
                  , usdFee = sum (map snd usdFeeL)
                  }

-- | Submit an order and return 'OrderStats'. In case of some non-critical
-- errors things are re-tried automatically, but if API errors happen or network
-- errors occur during critical phases (like placing the order) a 'Left' with
-- the error is returned. Should not block longer than about 3 minutes.
submitOrder :: Maybe WatchdogLogger-> CurlHandle -> MtGoxCredentials-> OrderType-> Integer-> IO (Either String OrderStats)
submitOrder mLogger curlHandle mtGoxCreds orderType amount = runEitherT $ do
    -- step 1: make sure network connection is present
    --         and no orders are pending
    EitherT $ letOrdersExecuteR mLogger curlHandle mtGoxCreds
    -- step 2: submit order
    order <- EitherT $ case orderType of
                        OrderTypeBuyBTC ->
                            submitBtcBuyOrder curlHandle mtGoxCreds amount
                        OrderTypeSellBTC ->
                            submitBtcSellOrder curlHandle mtGoxCreds amount
    -- step 3: wait for order to complete
    r <- liftIO $ letOrdersExecuteR mLogger curlHandle mtGoxCreds
    case r of
        Left errMsg -> left $ "Warning: After submitting order the call"
                                ++ " to letOrdersExecuteR failed ("
                                ++ errMsg ++ ")"
        Right _ -> return ()
    -- step 4: get trade ids
    let orderID = oOrderID order
    orderResult <- EitherT $ getOrderResultR mLogger curlHandle mtGoxCreds
                                                orderType orderID
    let tradeIDs = orTradeIDs orderResult
    -- step 5: collect wallet entries for all trade ids
    histories <- forM tradeIDs $ \tradeID -> EitherT (getWalletHistory tradeID)
    return $ processWalletHistories histories
  where
    getWalletHistory = getWalletHistoryR mLogger curlHandle mtGoxCreds
