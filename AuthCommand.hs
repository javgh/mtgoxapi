{-# LANGUAGE OverloadedStrings #-}
module AuthCommand
    ( AuthCommand (..)
    , prepareAuthCommand
    , getNonce
    ) where

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

dash :: Word8
dash = B.head "-"

hardcodedAuthKey :: B.ByteString
hardcodedAuthKey = "5be9c2b8-6f85-408d-a8de-18db2f5fc3af"

hardcodedAuthSecret :: B.ByteString
hardcodedAuthSecret = "A0seCE45EgkyUOliU3wokMuRwq/AbdpVMJ353kmDXtH0mWdy9QqsSKVkAer1AbfG4bYUO4YKVyh2ovO4pR1lag=="

data AuthCommand = AuthCommand { acCall :: T.Text
                               , acParameters :: [(T.Text, T.Text)]
                               , acSetBTCUSD :: Bool
                               }
                   deriving (Show)

getNonce :: IO T.Text
getNonce = do
    now <- getPOSIXTime
    let nonce = T.pack . show . round $ now * 1000000
    return nonce

prepareCallPayload :: ToJSON a => AuthCommand -> a -> Value
prepareCallPayload (AuthCommand { acCall = acCall
                               , acParameters = acParameters
                               , acSetBTCUSD = acSetBTCUSD
                               }) nonce =
    let alwaysPresent = [ "id" .= nonce
                        , "call" .= acCall
                        , "nonce" .= nonce
                        , "params" .= toMap acParameters
                        ]
        optionalAddon = if acSetBTCUSD
                            then [ "item" .= ("BTC" :: T.Text)
                                 , "currency" .= ("USD" :: T.Text)
                                 ]
                            else []
    in object (alwaysPresent ++ optionalAddon)
  where
    toMap :: ToJSON b => [(T.Text, b)] -> Value
    toMap = object . map (uncurry (.=))

fromRight :: String -> Either t t1 -> t1
fromRight msg (Left _) = error msg
fromRight _ (Right a) = a

fromFst :: String -> (B.ByteString, B.ByteString) -> B.ByteString
fromFst msg (a, b)
  | B.null b  = a
  | otherwise = error msg

createSignedCall :: ToJSON a => AuthCommand -> a -> B.ByteString
createSignedCall authCmd nonce =
    let errMsg arg base = arg ++ " needs to be encoded in base " ++ base
        authKeyFiltered = B.filter (/= dash) hardcodedAuthKey
        authKeyDecoded = BL.fromChunks [ fromFst (errMsg "authKey" "16") $
                                         B16.decode authKeyFiltered ]
        authSecretDecoded = BL.fromChunks [ fromRight (errMsg "authSecret" "64") $
                                            B64.decode hardcodedAuthSecret ]
        call = encode $ prepareCallPayload authCmd nonce
        hmac = bytestringDigest $ hmacSha512 authSecretDecoded call
        payload = authKeyDecoded `BL.append` hmac `BL.append` call
    in B64.encode . foldl1 B.append $ BL.toChunks payload

prepareAuthCommand :: ToJSON a => AuthCommand -> a -> Value
prepareAuthCommand authCmd nonce =
    let signedCall = createSignedCall authCmd nonce
    in object [ "op" .= ("call" :: T.Text)
              , "id" .= nonce
              , "call" .= signedCall
              , "context" .= ("mtgox.com" :: T.Text)
              ]
