{-# LANGUAGE OverloadedStrings #-}

module Network.MtGoxAPI.Credentials
    ( initMtGoxCredentials
    , MtGoxCredentials(..)
    ) where

import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64

data MtGoxCredentials = MtGoxCredentials { mgcAuthKey :: B.ByteString
                                         , mgcAuthKeyDecoded :: B.ByteString
                                         , mgcAuthSecret :: B.ByteString
                                         , mgcAuthSecretDecoded :: B.ByteString
                                         }
                        deriving (Show)

dash :: Word8
dash = B.head "-"

fromRight :: String -> Either t t1 -> t1
fromRight msg (Left _) = error msg
fromRight _ (Right a) = a

fromFst :: String -> (B.ByteString, B.ByteString) -> B.ByteString
fromFst msg (a, b)
  | B.null b  = a
  | otherwise = error msg

initMtGoxCredentials :: B.ByteString -> B.ByteString -> MtGoxCredentials
initMtGoxCredentials authKey authSecret =
    let errMsg arg base = arg ++ " needs to be encoded in base " ++ base
        authKeyFiltered = B.filter (/= dash) authKey
        authKeyDecoded = fromFst (errMsg "authKey" "16")
                            . B16.decode $ authKeyFiltered
        authSecretDecoded = fromRight (errMsg "authSecret" "64")
                                . B64.decode $ authSecret
    in MtGoxCredentials { mgcAuthKey = authKey
                        , mgcAuthKeyDecoded = authKeyDecoded
                        , mgcAuthSecret = authSecret
                        , mgcAuthSecretDecoded = authSecretDecoded
                        }
