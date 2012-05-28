{-# LANGUAGE OverloadedStrings #-}
module AuthCommandReplyParsing
    ( PrivateInfoReply (..)
    , parseAuthCommandReply
    ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.String

import qualified Data.Attoparsec as AP
import qualified Data.Map as M
import qualified Data.Text as T

data PrivateInfoReply = PrivateInfoReply { pirBtcBalance :: Integer
                                         , pirUsdBalance :: Integer
                                         }
                        deriving (Show)

instance FromJSON PrivateInfoReply
  where
    parseJSON (Object o) = case extractBalances o of
        Just (btcV, usdV) -> do
            btcS <- parseJSON btcV :: Parser String
            usdS <- parseJSON usdV :: Parser String
            return PrivateInfoReply { pirBtcBalance = read btcS
                                    , pirUsdBalance = read usdS
                                    }
        Nothing -> mzero
    parseJSON _ = mzero

extractBalances :: (IsString k, Ord k) => M.Map k Value -> Maybe (Value, Value)
extractBalances o = do
    btc <- extractBalance "BTC" o
    usd <- extractBalance "USD" o
    return (btc, usd)

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

parseAuthCommandReply :: Value -> Result PrivateInfoReply
parseAuthCommandReply = fromJSON
