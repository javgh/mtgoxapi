{-# LANGUAGE OverloadedStrings #-}
module AuthCommandReplyParsing
    ( PrivateInfoReply (..)
    , FullDepthReply (..)
    , DepthEntry (..)
    , OrderReply (..)
    , IDKeyReply (..)
    , OpenOrderCountReply (..)
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

data OrderReply = OrderReply { orGUID :: T.Text }
                  deriving (Show)

data OpenOrderCountReply = OpenOrderCountReply { oocrCount :: Integer }
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
    parseJSON (String guid) = return $ OrderReply guid
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
