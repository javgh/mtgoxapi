{-# LANGUAGE OverloadedStrings #-}
module AuthCommandReplyParsing
    ( PrivateInfoReply (..)
    , parsePrivateInfoReply
    , parseFullDepthReply
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.String

import qualified Data.Attoparsec as AP
import qualified Data.Map as M
import qualified Data.Text as T

data PrivateInfoReply = PrivateInfoReply { pirBtcBalance :: Integer
                                         , pirUsdBalance :: Integer
                                         , pirBtcOperations :: Integer
                                         , pirUsdOperations :: Integer
                                         }
                        deriving (Show)

data DepthEntry = DepthEntry { deAmount :: Integer
                             , dePrice :: Integer
                             , deStamp :: T.Text
                             }
                  deriving (Show)

data FullDepthReply = FullDepthReply { fdrAsks :: [DepthEntry]
                                     , fdrBids :: [DepthEntry]
                                     }
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

extractBalancesAndOps :: (IsString k, Ord k) =>M.Map k Value -> Maybe (Value, Value, Value, Value)
extractBalancesAndOps o = do
    btc <- extractBalance "BTC" o
    usd <- extractBalance "USD" o
    btcOps <- extractOperations "BTC" o
    usdOps <- extractOperations "USD" o
    return (btc, usd, btcOps, usdOps)

extractOperations :: (IsString k, Ord k) => T.Text -> M.Map k Value -> Maybe Value
extractOperations currency o =
    M.lookup "Wallets" o
        >>= extractObject
        >>= M.lookup currency
        >>= extractObject
        >>= M.lookup "Operations"

extractBalance :: (IsString k, Ord k) => T.Text -> M.Map k Value -> Maybe Value
extractBalance currency o = do
    balance <- M.lookup "Wallets" o
                >>= extractObject
                >>= M.lookup currency
                >>= extractObject
                >>= M.lookup "Balance"
                >>= extractObject
    M.lookup "value_int" balance

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

coerceFromString :: Parser String -> Parser Integer
coerceFromString = fmap read

parsePrivateInfoReply :: Value -> Result PrivateInfoReply
parsePrivateInfoReply = fromJSON

parseFullDepthReply :: Value -> Result FullDepthReply
parseFullDepthReply = fromJSON
