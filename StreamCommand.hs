{-# LANGUAGE OverloadedStrings #-}
module StreamCommand
    ( encodeStreamCommand
    , StreamCommand (..)
    , StreamCommandNR (..)
    , StreamCommandWR (..)
    , StreamWriter
    ) where

import Data.Aeson
import Data.Aeson.Types

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import AuthCommand
import AuthCommandReplyParsing

data StreamCommand = StreamCommandNoReply { scnrCmd :: StreamCommandNR }
                   | StreamCommandWithReply {scwrCmd :: StreamCommandWR }
                   deriving (Show)

data StreamCommandNR = UnsubscribeCmd { scChannel :: T.Text }
                       | PrivateSubscribeCmd { scKey :: T.Text }
                       deriving (Show)

data StreamCommandWR = PrivateInfo { scNonce :: T.Text }
                     | USDCurrencyInfo { scNonce :: T.Text }
                     | IDKey { scNonce :: T.Text }
                     | FullDepth { scNonce :: T.Text }
                     | Order { scNonce :: T.Text
                             , scOrderType :: OrderType
                             , scAmount :: Integer
                             }
                     | OpenOrderCount { scNonce :: T.Text }
                     | WithdrawBTC { scNonce :: T.Text
                                   , scAddress :: T.Text
                                   , scAmount :: Integer
                                   }
                     deriving (Show)

type StreamWriter = StreamCommand -> IO ()

instance ToJSON StreamCommand
  where
    toJSON StreamCommandNoReply { scnrCmd = cmd} = toJSON cmd
    toJSON StreamCommandWithReply { scwrCmd = cmd } = toJSON cmd

instance ToJSON StreamCommandNR
  where
    toJSON cmd@UnsubscribeCmd {} =
        object [ "op" .= ("unsubscribe" :: T.Text)
               , "channel" .= scChannel cmd
               ]
    toJSON cmd@PrivateSubscribeCmd {} =
        object [ "op" .= ("mtgox.subscribe" :: T.Text)
               , "key" .= scKey cmd
               ]

instance ToJSON StreamCommandWR
  where
    toJSON cmd@PrivateInfo {} =
        let privateInfoCmd = AuthCommand { acCall = "private/info"
                                         , acParameters = []
                                         , acSetBTCUSD = False
                                         }
        in prepareAuthCommand privateInfoCmd (scNonce cmd)
    toJSON cmd@USDCurrencyInfo {} =
        let usdCurrencyInfoCmd = AuthCommand { acCall = "public/currency"
                                             , acParameters = []
                                             , acSetBTCUSD = True
                                             }
        in prepareAuthCommand usdCurrencyInfoCmd (scNonce cmd)
    toJSON cmd@IDKey {} =
        let idKeyCmd = AuthCommand { acCall = "private/idkey"
                                   , acParameters = []
                                   , acSetBTCUSD = False
                                   }
        in prepareAuthCommand idKeyCmd (scNonce cmd)
    toJSON cmd@FullDepth {} =
        let fullDepthCmd = AuthCommand { acCall = "public/fulldepth"
                                       , acParameters = []
                                       , acSetBTCUSD = True
                                       }
        in prepareAuthCommand fullDepthCmd (scNonce cmd)
    toJSON cmd@Order {} =
        let params = [ ("type", case scOrderType cmd of
                                    OrderTypeBuyBTC -> "bid"
                                    OrderTypeSellBTC -> "ask")
                     , ("amount_int", T.pack . show $ scAmount cmd)
                     , ("prefer_fiat_fee", "1")
                     ]
            orderCmd = AuthCommand { acCall = "private/order/add"
                                   , acParameters = params
                                   , acSetBTCUSD = True
                                   }
        in prepareAuthCommand orderCmd (scNonce cmd)
    toJSON cmd@OpenOrderCount {} =
        let openOrderCountCmd = AuthCommand { acCall = "private/orders"
                                            , acParameters = []
                                            , acSetBTCUSD = False
                                            }
        in prepareAuthCommand openOrderCountCmd (scNonce cmd)
    toJSON cmd@WithdrawBTC {} =
        let params = [ ("btca", scAddress cmd)
                     , ("amount", T.pack . show $ scAmount cmd)
                     ]
            withdrawBTCCmd = AuthCommand { acCall = "private/withdraw"
                                         , acParameters = params
                                         , acSetBTCUSD = False
                                         }
        in prepareAuthCommand withdrawBTCCmd (scNonce cmd)

encodeStreamCommand :: StreamCommand -> L.ByteString
encodeStreamCommand = encode
