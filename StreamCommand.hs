{-# LANGUAGE OverloadedStrings #-}
module StreamCommand
    ( encodeStreamCommand
    , StreamCommand (..)
    , StreamWriter
    ) where

import Data.Aeson
import Data.Aeson.Types

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import AuthCommand

data StreamCommand = UnsubscribeCmd { ucChannel :: T.Text }
                   | PrivateInfo { piNonce :: T.Text }
                   deriving (Show)

type StreamWriter = StreamCommand -> IO ()

instance ToJSON StreamCommand
  where
    toJSON cmd@UnsubscribeCmd {} =
        object [ "op" .= ("unsubscribe" :: T.Text)
               , "channel" .= ucChannel cmd
               ]
    toJSON cmd@PrivateInfo {} =
        let privateInfoCmd = AuthCommand { acCall = "private/info"
                                         , acParameters = []
                                         , acIsGeneric = True
                                         }
        in prepareAuthCommand privateInfoCmd (piNonce cmd)

encodeStreamCommand :: StreamCommand -> L.ByteString
encodeStreamCommand = encode
