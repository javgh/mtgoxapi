{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Framework

import Network.MtGoxAPI.DepthStore.Tests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = depthStoreTests
