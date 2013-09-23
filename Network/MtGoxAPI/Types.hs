{-# LANGUAGE OverloadedStrings #-}
module Network.MtGoxAPI.Types
    ( parseStreamLine
    , StreamMessage(..)
    , MtGoxStreamSettings(..)
    , WalletNotifierSetting(..)
    , FullDepthSetting(..)
    , DepthType(..)
    , WalletOperationType(..)
    , IDKey(..)
    , FullDepth(..)
    , DepthEntry(..)
    , OpenOrderCount(..)
    , Order(..)
    , OrderType(..)
    , OrderID(..)
    , OrderResult(..)
    , TradeID(..)
    , WalletHistory(..)
    , PrivateInfo(..)
    , BitcoinDepositAddress(..)
    , BitcoinAddress(..)
    , WithdrawResult(..)
    , WalletEntry(..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Hashable
import Data.String

import qualified Data.Attoparsec as AP
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

data DepthType = Ask | Bid
                 deriving (Eq, Show)

data WalletOperationType = BTCDeposit
                         | BTCWithdraw
                         | USDEarned
                         | USDSpent
                         | BTCIn
                         | BTCOut
                         | USDFee
                         deriving (Eq, Show)

data MtGoxStreamSettings = MtGoxStreamSettings WalletNotifierSetting
                                                    FullDepthSetting
                          deriving (Show)

data WalletNotifierSetting = EnableWalletNotifications
                           | DisableWalletNotifications
                           deriving (Eq, Show)

data FullDepthSetting = RequestFullDepth
                      | SkipFullDepth
                      deriving (Show)

data StreamMessage = TickerUpdateUSD { tuBid :: Integer
                                     , tuAsk :: Integer
                                     , tuLast :: Integer
                                     }
                     | DepthUpdateUSD { duPrice :: Integer
                                      , duVolume :: Integer
                                      , duType :: DepthType
                                      }
                     | WalletOperation { woType :: WalletOperationType
                                       , woAmount :: Integer
                                       }
                     | Subscribed { sChannel :: T.Text }
                     | Unsubscribed { usChannel :: T.Text }
                     | CallResult { crID :: T.Text
                                  , crResult :: Value
                                  }
                     | OtherMessage
                     deriving (Eq, Show)

data PrivateInfo = PrivateInfo { piBtcBalance :: Integer
                               , piUsdBalance :: Integer
                               , piBtcOperations :: Integer
                               , piUsdOperations :: Integer
                               , piFee :: Double
                               }
                   deriving (Show)

data IDKey = IDKey { idkKey :: T.Text }
             deriving (Show)

data DepthEntry = DepthEntry { deAmount :: Integer
                             , dePrice :: Integer
                             , deStamp :: T.Text
                             }
                  deriving (Show, Read)

data FullDepth = FullDepth { fdAsks :: [DepthEntry]
                           , fdBids :: [DepthEntry]
                           }
                 deriving (Show, Read)

data OrderID = OrderID { oid :: T.Text }
               deriving (Show)

data TradeID = TradeID { tid :: T.Text }
               deriving (Show)

data Order = Order { oOrderID :: OrderID }
             deriving (Show)

data OrderResult = OrderResult { orTradeIDs :: [TradeID] }
                   deriving (Show)

data OpenOrderCount = OpenOrderCount { oocCount :: Integer }
                      deriving (Show)

data OrderType = OrderTypeBuyBTC | OrderTypeSellBTC
                 deriving (Show)

data WalletEntry = WalletEntry { weDate :: Integer
                               , weType :: WalletOperationType
                               , weAmount :: Integer
                               , weBalance :: Integer
                               , weInfo :: T.Text
                               }
                   deriving (Show)

data WalletHistory = WalletHistory { whEntries :: [WalletEntry] }
                     deriving (Show)

data BitcoinAddress = BitcoinAddress { baAddress :: T.Text }
                      deriving (Show)

data BitcoinDepositAddress = BitcoinDepositAddress { bdaAddr :: BitcoinAddress }
                             deriving (Show)

data WithdrawResult = WithdrawResult { wsTxID :: T.Text }
                      deriving (Show)

instance FromJSON StreamMessage
  where
    parseJSON (Object o) = case getOperation o of
        Just ("subscribe", subscribe) ->
            Subscribed <$> subscribe .: "channel"
        Just ("unsubscribe", unsubscribe) ->
            Unsubscribed <$> unsubscribe .: "channel"
        Just ("result", result) ->
            CallResult <$> result .: "id" <*> result .: "result"
        Just ("ticker", ticker) -> parseTicker ticker
        Just ("depth", depth) -> parseDepth depth
        Just ("wallet", wallet) -> parseWallet wallet
        Just _ -> return OtherMessage
        Nothing -> return OtherMessage
    parseJSON _ = mzero

getOperation :: H.HashMap T.Text Value -> Maybe (T.Text, Object)
getOperation o = do
    op' <- H.lookup "op" o >>= extractText
    case op' of
        "private" -> do
            op <- H.lookup "private" o >>= extractText
            payload <- H.lookup op o >>= extractObject
            return (op, payload)
        "subscribe" -> return (op', o)
        "unsubscribe" -> return (op', o)
        "result" -> return (op', o)
        _ -> fail "unknown operation"

parseWallet :: Object -> Parser StreamMessage
parseWallet wallet = do
    op <- wallet .: "op" :: Parser String
    case op of
        "deposit" -> go BTCDeposit "BTC"
        "withdraw" -> go BTCWithdraw "BTC"
        "earned" -> go USDEarned "EUR"
        "spent" -> go USDSpent "EUR"
        "in" -> go BTCIn "BTC"
        "out" -> go BTCOut "BTC"
        "fee" -> go USDFee "EUR"
        _ -> return OtherMessage
  where
    go checkedOp expCurrency = do
        amountDetails <- wallet .: "amount"
        amount <- coerceFromString $ amountDetails .: "value_int"
        currency <- amountDetails .: "currency" :: Parser String
        if currency == expCurrency
            then return WalletOperation { woType = checkedOp
                                        , woAmount = amount
                                        }
            else mzero

parseDepth :: (Eq k, IsString k, Hashable k) =>H.HashMap k Value -> Parser StreamMessage
parseDepth depth = case extractDepthData depth of
    Just (price, volume, depthType) ->
        DepthUpdateUSD <$> coerceFromString (parseJSON price)
                       <*> coerceFromString (parseJSON volume)
                       <*> pure depthType
    Nothing -> mzero

extractDepthData :: (Eq k, IsString k, Hashable k) =>H.HashMap k Value -> Maybe (Value, Value, DepthType)
extractDepthData o = do
    currency <- H.lookup "currency" o
    guard (currency == expectedCurrency)
    price <- H.lookup "price_int" o
    volume <- H.lookup "total_volume_int" o
    depthType <- H.lookup "type_str" o >>= convertTypeStr
    return (price, volume, depthType)

convertTypeStr :: Value -> Maybe DepthType
convertTypeStr (String "ask") = Just Ask
convertTypeStr (String "bid") = Just Bid
convertTypeStr _ = Nothing

parseTicker :: (Eq k, IsString k, Hashable k) =>H.HashMap k Value -> Parser StreamMessage
parseTicker ticker = case extractTickerData ticker of
    Just (buy, sell, lst) ->
        TickerUpdateUSD <$> coerceFromString (parseJSON buy)
                        <*> coerceFromString (parseJSON sell)
                        <*> coerceFromString (parseJSON lst)
    Nothing -> mzero

coerceFromString :: Parser String -> Parser Integer
coerceFromString = fmap read

extractTickerData :: (Eq k, IsString k, Hashable k) =>H.HashMap k Value -> Maybe (Value, Value, Value)
extractTickerData o = do
    buyPrice <- lookupInTicker "buy" o
    sellPrice <- lookupInTicker "sell" o
    lastPrice <- lookupInTicker "last" o
    return (buyPrice, sellPrice, lastPrice)

lookupInTicker :: (Eq k, Hashable k) => k -> H.HashMap k Value -> Maybe Value
lookupInTicker field o = do
        tickerField <- H.lookup field o
                        >>= extractObject
        currency <- H.lookup "currency" tickerField
        guard (currency == expectedCurrency)
        H.lookup "value_int" tickerField

extractObject :: Value -> Maybe Object
extractObject (Object o) = Just o
extractObject _ = Nothing

extractText :: Value -> Maybe T.Text
extractText (String s) = Just s
extractText _ = Nothing

expectedCurrency :: Value
expectedCurrency = "EUR"

parseLine :: B.ByteString -> Either String StreamMessage
parseLine = collapseErrors . parseStreamMessage
  where
    parseStreamMessage :: B.ByteString -> Either String (Result StreamMessage)
    parseStreamMessage = AP.parseOnly (fromJSON <$> json)

collapseErrors :: Either String (Result b) -> Either String b
collapseErrors (Left err) = Left err
collapseErrors (Right (Error err)) = Left err
collapseErrors (Right (Success payload)) = Right payload

parseStreamLine :: B.ByteString -> StreamMessage
parseStreamLine line = case parseLine line of
    Right msg -> msg
    Left _ -> OtherMessage

instance FromJSON PrivateInfo
  where
    parseJSON (Object o) = case extractBalancesAndOps o of
        Just (btcV, usdV, btcOps, usdOps) -> do
            btcS <- parseJSON btcV :: Parser String
            usdS <- parseJSON usdV :: Parser String
            btcOpsI <- parseJSON btcOps
            usdOpsI <- parseJSON usdOps
            fee <- o .: "Trade_Fee"
            return PrivateInfo { piBtcBalance = read btcS
                               , piUsdBalance = read usdS
                               , piBtcOperations = btcOpsI
                               , piUsdOperations = usdOpsI
                               , piFee = fee
                               }
        Nothing -> mzero
    parseJSON _ = mzero

extractBalancesAndOps :: (Eq k, IsString k, Hashable k) =>H.HashMap k Value -> Maybe (Value, Value, Value, Value)
extractBalancesAndOps o = do
    btc <- extractBalance "BTC" o
    usd <- extractBalance "EUR" o
    btcOps <- extractOperations "BTC" o
    usdOps <- extractOperations "EUR" o
    return (btc, usd, btcOps, usdOps)

extractOperations :: (Eq k, IsString k, Hashable k) =>T.Text -> H.HashMap k Value -> Maybe Value
extractOperations currency o =
    H.lookup "Wallets" o
        >>= extractObject
        >>= H.lookup currency
        >>= extractObject
        >>= H.lookup "Operations"

extractBalance :: (Eq k, IsString k, Hashable k) =>T.Text -> H.HashMap k Value -> Maybe Value
extractBalance currency o = do
    balance <- H.lookup "Wallets" o
                >>= extractObject
                >>= H.lookup currency
                >>= extractObject
                >>= H.lookup "Balance"
                >>= extractObject
    H.lookup "value_int" balance

instance FromJSON DepthEntry
  where
    parseJSON (Object o) =
        DepthEntry <$> coerceFromString (o .: "amount_int")
                   <*> coerceFromString (o .: "price_int")
                   <*> o .: "stamp"
    parseJSON _ = mzero

instance FromJSON FullDepth
  where
    parseJSON (Object o) =
        FullDepth <$> o .: "asks"
                  <*> o .: "bids"
    parseJSON _ = mzero

instance FromJSON Order
  where
    parseJSON (String guid) = return $ Order (OrderID guid)
    parseJSON _ = mzero

instance FromJSON IDKey
  where
    parseJSON (String key) = return $ IDKey key
    parseJSON _ = mzero

instance FromJSON OpenOrderCount
  where
    parseJSON (Array orders) =
        return $ OpenOrderCount (fromIntegral . V.length $ orders)
    parseJSON _ = mzero

instance FromJSON OrderResult
  where
    parseJSON (Object o) = case H.lookup "trades" o of
        Just (Array trades) -> do
            ids <- mapM extractTradeID (V.toList trades)
            return $ OrderResult ids
        Just _ -> mzero
        Nothing -> mzero
    parseJSON _ = mzero

extractTradeID :: MonadPlus m => Value -> m TradeID
extractTradeID (Object o) = case H.lookup "trade_id" o of
    Just (String tradeID) -> return $ TradeID tradeID
    Just _ -> mzero
    Nothing -> mzero
extractTradeID _ = mzero

instance FromJSON WalletEntry
  where
    parseJSON (Object o) = do
        date <- o .: "Date"
        typeString <- o .: "Type" :: Parser T.Text
        entryType <- case typeString of
                        "fee" -> return USDFee
                        "earned" -> return USDEarned
                        "spent" -> return USDSpent
                        _ -> mzero
        amount <- coerceFromString (o .: "Value" >>=
                                        \v -> v .: "value_int")
        balance <- coerceFromString (o .: "Balance" >>=
                                        \v -> v .: "value_int")
        info <- o .: "Info"
        return $ WalletEntry date entryType amount balance info
    parseJSON _ = mzero

instance FromJSON WalletHistory
  where
    parseJSON (Object o) = case H.lookup "result" o of
        Just results -> do
            entries <- parseJSON results :: Parser [WalletEntry]
            return $ WalletHistory entries
        Nothing -> mzero
    parseJSON _ = mzero

instance FromJSON BitcoinAddress
  where
    parseJSON v = BitcoinAddress <$> parseJSON v

instance FromJSON BitcoinDepositAddress
  where
    parseJSON (Object o) = BitcoinDepositAddress <$> o .: "addr"
    parseJSON _ = mzero

instance FromJSON WithdrawResult
  where
    parseJSON (Object o) = WithdrawResult <$> o .: "trx"
    parseJSON _ = mzero
