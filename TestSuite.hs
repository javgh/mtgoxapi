{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Network.MtGoxAPI.DepthStore.Tests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = depthStoreTests
