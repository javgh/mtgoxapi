{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Error
import Control.Monad.IO.Class
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Network.MtGoxAPI
import Network.MtGoxAPI.Handles
import Network.MtGoxAPI.Credentials
import Network.MtGoxAPI.DepthStore.Tests

test1 :: MtGoxAPIHandles -> Test
test1 apiData = testCase "getOrderCountR returns data" $ do
    r <- callHTTPApi apiData getOrderCountR
    case r of
        Just _ -> return ()
        Nothing -> assertFailure "getOrderCountR did not return data."

test2 :: MtGoxAPIHandles -> Test
test2 apiData = testCase "getPrivateInfoR returns data" $ do
    r <- callHTTPApi apiData getPrivateInfoR
    case r of
        Just _ -> return ()
        Nothing -> assertFailure "getPrivateInfoR did not return data."

test3 :: MtGoxAPIHandles -> Test
test3 apiData = testCase "getBitcoinDepositAddressR returns data" $ do
    r <- callHTTPApi apiData getBitcoinDepositAddressR
    case r of
        Just _ -> return ()
        Nothing -> assertFailure "getBitcoinDepositAddressR did not return data."

test4 :: MtGoxAPIHandles -> Test
test4 apiData = testCase "ticker is available" $ do
    r <- getTickerStatus (mtgoxTickerMonitorHandle apiData)
    case r of
        TickerStatus{} -> return ()
        TickerUnavailable -> assertFailure "Ticker is not available."

test5 :: MtGoxAPIHandles -> Test
test5 apiData = testCase "simulateBTCSell returns non-zero amount" $ do
    r <- simulateBTCSell (mtgoxDepthStoreHandle apiData) 1000000
    case r of
        Nothing -> assertFailure "Simulation did not return any data."
        Just v -> assertBool "value is zero" (v /= 0)

test6 :: MtGoxAPIHandles -> Test
test6 apiData = testCase "simulateBTCBuy returns non-zero amount" $ do
    r <- simulateBTCBuy (mtgoxDepthStoreHandle apiData) 1000000
    case r of
        Nothing -> assertFailure "Simulation did not return any data."
        Just v -> assertBool "value is zero" (v /= 0)

test7 :: MtGoxAPIHandles -> Test
test7 apiData = testCase "simulateUSDSell returns non-zero amount" $ do
    r <- simulateUSDSell (mtgoxDepthStoreHandle apiData) 1000
    case r of
        Nothing -> assertFailure "Simulation did not return any data."
        Just v -> assertBool "value is zero" (v /= 0)

test8 :: MtGoxAPIHandles -> Test
test8 apiData = testCase "two simulations mostly match" $ do
    let startValue = 1000000
    m <- runMaybeT $ do
        usdValue <- MaybeT $ simulateBTCBuy
                                (mtgoxDepthStoreHandle apiData) startValue
        btcValue <- MaybeT $ simulateUSDSell
                                (mtgoxDepthStoreHandle apiData) usdValue
        let diff = abs (startValue - btcValue)
        liftIO $ assertBool "values differ more than 0.0001 BTC"
                    (diff <= 10000)
    case m of
        Nothing -> assertFailure "Simulation did not return any data."
        Just _ -> return ()

mtgoxAPITests ::  [MtGoxAPIHandles -> Test]
mtgoxAPITests = [test1, test2, test3, test4, test5, test6, test7, test8]

tests :: [Test]
tests = depthStoreTests

main :: IO ()
main = do
    apiData <- initMtGoxAPI (Just silentLogger) debugCredentials SkipFullDepth
    let apiTests = map ($ apiData) mtgoxAPITests
    defaultMain $ apiTests ++ tests
