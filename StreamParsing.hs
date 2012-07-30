{-# LANGUAGE OverloadedStrings #-}
module StreamParsing
    ( parseStreamLine
    , StreamMessage(..)
    , DepthType(..)
    , WalletOperationType(..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Hashable
import Data.String
import Network

import qualified Data.Attoparsec as AP
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified System.IO as IO

data DepthType = Ask | Bid
                 deriving (Show)

data WalletOperationType = BTCDeposit
                         | BTCWithdraw
                         | USDEarned
                         | USDSpent
                         | BTCIn
                         | BTCOut
                         | USDFee
                         deriving (Eq, Show)

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
        "earned" -> go USDEarned "USD"
        "spent" -> go USDSpent "USD"
        "in" -> go BTCIn "BTC"
        "out" -> go BTCOut "BTC"
        "fee" -> go USDFee "USD"
        _ -> return OtherMessage
  where
    go checkedOp expectedCurrency = do
        amountDetails <- wallet .: "amount"
        amount <- coerceFromString $ amountDetails .: "value_int"
        currency <- amountDetails .: "currency" :: Parser String
        if currency == expectedCurrency
            then return $ WalletOperation { woType = checkedOp
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
    Just (buy, sell, last) ->
        TickerUpdateUSD <$> coerceFromString (parseJSON buy)
                        <*> coerceFromString (parseJSON sell)
                        <*> coerceFromString (parseJSON last)
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
expectedCurrency = "USD"

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
