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

data StreamCommand = StreamCommandNoReply { scnrCmd :: StreamCommandNR }
                   | StreamCommandWithReply {scwrCmd :: StreamCommandWR }
                   deriving (Show)

data StreamCommandNR = UnsubscribeCmd { scChannel :: T.Text }
                       deriving (Show)

data StreamCommandWR = PrivateInfo { scNonce :: T.Text }
                     | USDCurrencyInfo { scNonce :: T.Text }
                     | IDKey { scNonce :: T.Text }
                     | FullDepth { scNonce :: T.Text }
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

encodeStreamCommand :: StreamCommand -> L.ByteString
encodeStreamCommand = encode
