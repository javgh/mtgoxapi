{-# LANGUAGE OverloadedStrings #-}
module Network.MtGoxAPI.StreamCommands
    ( encodeStreamCommand
    , StreamCommand (..)
    , module Network.MtGoxAPI.StreamAuthCommands
    ) where

import Data.Aeson

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Network.MtGoxAPI.Credentials
import Network.MtGoxAPI.StreamAuthCommands

data StreamCommand = UnsubscribeCmd { scChannel :: T.Text }
                     | SubscribeCmd { scChannel :: T.Text }
                     | PrivateSubscribeCmd { scKey :: T.Text }
                     | IDKeyCmd
                     | FullDepthCmd
                     deriving (Show)

encodeStreamCommand :: StreamCommand -> MtGoxCredentials -> IO L.ByteString
encodeStreamCommand cmd creds = do
    nonce <- getNonce
    let v = case cmd of
                UnsubscribeCmd { scChannel = channel} ->
                    object [ "op" .= ("unsubscribe" :: T.Text)
                           , "channel" .= channel
                           ]
                SubscribeCmd { scChannel = channel} ->
                    object [ "op" .= ("mtgox.subscribe" :: T.Text)
                           , "channel" .= channel
                           ]
                PrivateSubscribeCmd { scKey = key } ->
                    object [ "op" .= ("mtgox.subscribe" :: T.Text)
                           , "key" .= key
                           ]
                IDKeyCmd ->
                    let idKeyCmd =
                            StreamAuthCommandData { sacdCall = "private/idkey"
                                                  , sacdParameters = []
                                                  , sacdSetBTCUSD = False
                                                  , sacdNonce = nonce
                                                  }
                    in prepareAuthCommand creds idKeyCmd
                FullDepthCmd ->
                    let fullDepthCmd =
                            StreamAuthCommandData { sacdCall = "public/fulldepth"
                                                  , sacdParameters = []
                                                  , sacdSetBTCUSD = True
                                                  , sacdNonce = nonce
                                                  }
                    in prepareAuthCommand creds fullDepthCmd
    return $ encode v
