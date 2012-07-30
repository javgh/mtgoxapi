{-# LANGUAGE OverloadedStrings #-}
module AuthCommandReplyParsing
    ( PrivateInfoReply (..)
    , FullDepthReply (..)
    , DepthEntry (..)
    , OrderReply (..)
    , IDKeyReply (..)
    , OpenOrderCountReply (..)
    , OrderType(..)
    , OrderResult(..)
    , OrderID(..)
    , TradeID(..)
    , WalletEntry(..)
    , WalletHistory(..)
    , parsePrivateInfoReply
    , parseFullDepthReply
    , parseOrderReply
    , parseIDKeyReply
    , parseOpenOrderCountReply
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.String
import Data.Hashable

import qualified Data.Attoparsec as AP
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import StreamParsing

data PrivateInfoReply = PrivateInfoReply { pirBtcBalance :: Integer
                                         , pirUsdBalance :: Integer
                                         , pirBtcOperations :: Integer
                                         , pirUsdOperations :: Integer
                                         }
                        deriving (Show)

data IDKeyReply = IDKeyReply { idkrKey :: T.Text }
                  deriving (Show)

data DepthEntry = DepthEntry { deAmount :: Integer
                             , dePrice :: Integer
                             , deStamp :: T.Text
                             }
                  deriving (Show, Read)

data FullDepthReply = FullDepthReply { fdrAsks :: [DepthEntry]
                                     , fdrBids :: [DepthEntry]
                                     }
                      deriving (Show, Read)

data OrderID = OrderID { oid :: T.Text }
               deriving (Show)

data TradeID = TradeID { tid :: T.Text }
               deriving (Show)

data OrderReply = OrderReply { orOrderID :: OrderID }
                  deriving (Show)

data OrderResult = OrderResult { orTradeIDs :: [TradeID] }
                   deriving (Show)

data OpenOrderCountReply = OpenOrderCountReply { oocrCount :: Integer }
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

instance FromJSON PrivateInfoReply
  where
    parseJSON (Object o) = case extractBalancesAndOps o of
        Just (btcV, usdV, btcOps, usdOps) -> do
            btcS <- parseJSON btcV :: Parser String
            usdS <- parseJSON usdV :: Parser String
            btcOpsI <- parseJSON btcOps
            usdOpsI <- parseJSON usdOps
            return PrivateInfoReply { pirBtcBalance = read btcS
                                    , pirUsdBalance = read usdS
                                    , pirBtcOperations = btcOpsI
                                    , pirUsdOperations = usdOpsI
                                    }
        Nothing -> mzero
    parseJSON _ = mzero

extractBalancesAndOps :: (Eq k, IsString k, Hashable k) =>H.HashMap k Value -> Maybe (Value, Value, Value, Value)
extractBalancesAndOps o = do
    btc <- extractBalance "BTC" o
    usd <- extractBalance "USD" o
    btcOps <- extractOperations "BTC" o
    usdOps <- extractOperations "USD" o
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

extractObject :: Value -> Maybe Object
extractObject (Object o) = Just o
extractObject _ = Nothing

instance FromJSON DepthEntry
  where
    parseJSON (Object o) =
        DepthEntry <$> coerceFromString (o .: "amount_int")
                   <*> coerceFromString (o .: "price_int")
                   <*> o .: "stamp"
    parseJSON _ = mzero

instance FromJSON FullDepthReply
  where
    parseJSON (Object o) =
        FullDepthReply <$> o .: "asks"
                       <*> o .: "bids"

instance FromJSON OrderReply
  where
    parseJSON (String guid) = return $ OrderReply (OrderID guid)
    parseJSON _ = mzero

instance FromJSON IDKeyReply
  where
    parseJSON (String key) = return $ IDKeyReply key
    parseJSON _ = mzero

instance FromJSON OpenOrderCountReply
  where
    parseJSON (Array orders) =
        return $ OpenOrderCountReply (fromIntegral . V.length $ orders)
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
    Just (String id) -> return $ TradeID id
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

coerceFromString :: Parser String -> Parser Integer
coerceFromString = fmap read

parsePrivateInfoReply :: Value -> Maybe PrivateInfoReply
parsePrivateInfoReply v = case fromJSON v of
                            Success p -> Just p
                            Error _ -> Nothing

parseFullDepthReply :: Value -> Maybe FullDepthReply
parseFullDepthReply v = case fromJSON v of
                            Success p -> Just p
                            Error _ -> Nothing

parseOrderReply :: Value -> Maybe OrderReply
parseOrderReply v = case fromJSON v of
                        Success p -> Just p
                        Error _ -> Nothing

parseIDKeyReply :: Value -> Maybe IDKeyReply
parseIDKeyReply v = case fromJSON v of
                        Success p -> Just p
                        Error _ -> Nothing

parseOpenOrderCountReply :: Value -> Maybe OpenOrderCountReply
parseOpenOrderCountReply v = case fromJSON v of
                                Success p -> Just p
                                Error _ -> Nothing
