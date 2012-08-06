{-# LANGUAGE OverloadedStrings #-}
module Network.MtGoxAPI.StreamAuthCommands
    ( StreamAuthCommandData(..)
    , prepareAuthCommand
    , getNonce
    , parseIDKeyCallResult
    , parseFullDepthCallResult
    ) where

-- Structure of an authenticated command send over the streaming API:
-- {
--     "op":"call"
--     "id": <nonce>
--     "call": base64-encoded string
--         <api key already decoded (remove dashes first)>
--         <hmac-signed copy of the following string>
--         <json array
--             {
--                 "id": <nonce>
--                 "call": <for example private/info>
--                 "nonce": <nonce>
--                 "params": <array with parameters>
--                 "item": BTC     ?
--                 "currency": USD     ?
--             }
--         >
--     "context":"mtgox.com"
-- }

import Data.Aeson
import Data.Aeson.Types
import Data.Digest.Pure.SHA
import Data.Time.Clock.POSIX
import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Network.MtGoxAPI.Credentials
import Network.MtGoxAPI.Types

data StreamAuthCommandData = StreamAuthCommandData
                                { sacdCall :: T.Text
                                , sacdParameters :: [(T.Text, T.Text)]
                                , sacdSetBTCUSD :: Bool
                                , sacdNonce :: T.Text
                                }
                             deriving (Show)

getNonce :: IO T.Text
getNonce = do
    now <- getPOSIXTime
    let nonce = T.pack . show . round $ now * 1000000
    return nonce

prepareCallPayload :: StreamAuthCommandData -> BL.ByteString
prepareCallPayload (StreamAuthCommandData { sacdCall = sacdCall
                                          , sacdParameters = sacdParameters
                                          , sacdSetBTCUSD = sacdSetBTCUSD
                                          , sacdNonce = sacdNonce
                                          }) =
    let alwaysPresent = [ "id" .= sacdNonce
                        , "call" .= sacdCall
                        , "nonce" .= sacdNonce
                        , "params" .= toMap sacdParameters
                        ]
        optionalAddon = if sacdSetBTCUSD
                            then [ "item" .= ("BTC" :: T.Text)
                                 , "currency" .= ("USD" :: T.Text)
                                 ]
                            else []
    in encode $ object (alwaysPresent ++ optionalAddon)
  where
    toMap :: ToJSON b => [(T.Text, b)] -> Value
    toMap = object . map (uncurry (.=))

createSignedCall :: MtGoxCredentials -> StreamAuthCommandData -> B.ByteString
createSignedCall creds authCmd =
    let authKeyDecoded = BL.fromChunks [mgcAuthKeyDecoded creds]
        authSecretDecoded = BL.fromChunks [mgcAuthSecretDecoded creds]
        call = prepareCallPayload authCmd
        hmac = bytestringDigest $ hmacSha512 authSecretDecoded call
        payload = authKeyDecoded `BL.append` hmac `BL.append` call
    in B64.encode . foldl1 B.append $ BL.toChunks payload

prepareAuthCommand :: MtGoxCredentials -> StreamAuthCommandData -> Value
prepareAuthCommand creds authCmd =
    let signedCall = createSignedCall creds authCmd
    in object [ "op" .= ("call" :: T.Text)
              , "id" .= sacdNonce authCmd
              , "call" .= signedCall
              , "context" .= ("mtgox.com" :: T.Text)
              ]

parseIDKeyCallResult :: StreamMessage -> Maybe IDKey
parseIDKeyCallResult CallResult { crResult = v } =
    case fromJSON v of
        Success p -> Just p
        Error _ -> Nothing
parseIDKeyCallResult _ = Nothing

parseFullDepthCallResult :: StreamMessage -> Maybe FullDepth
parseFullDepthCallResult CallResult { crResult = v } =
    case fromJSON v of
        Success p -> Just p
        Error _ -> Nothing
parseFullDepthCallResult _ = Nothing
