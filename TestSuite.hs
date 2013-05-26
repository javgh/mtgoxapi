{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Error
import Control.Monad.IO.Class
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Network.MtGoxAPI
import Network.MtGoxAPI.DepthStore.Tests

cent :: Integer
cent = 1000000

test1 :: MtGoxAPIHandles -> Test
test1 apiData = testCase "getOrderCountR returns data" $ do
    r <- getOrderCountR apiData
    case r of
        Right _ -> return ()
        Left _ -> assertFailure "getOrderCountR did not return data."

test2 :: MtGoxAPIHandles -> Test
test2 apiData = testCase "getPrivateInfoR returns data" $ do
    r <- getPrivateInfoR apiData
    case r of
        Right _ -> return ()
        Left _ -> assertFailure "getPrivateInfoR did not return data."

test3 :: MtGoxAPIHandles -> Test
test3 apiData = testCase "getBitcoinDepositAddressR returns data" $ do
    r <- getBitcoinDepositAddressR apiData
    case r of
        Right _ -> return ()
        Left _ -> assertFailure "getBitcoinDepositAddressR did not return data."

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
        DepthStoreAnswer v -> assertBool "value is zero" (v /= 0)
        _ -> assertFailure "Simulation did not return any data."

test6 :: MtGoxAPIHandles -> Test
test6 apiData = testCase "simulateBTCBuy returns non-zero amount" $ do
    r <- simulateBTCBuy (mtgoxDepthStoreHandle apiData) 1000000
    case r of
        DepthStoreAnswer v -> assertBool "value is zero" (v /= 0)
        _ -> assertFailure "Simulation did not return any data."

test7 :: MtGoxAPIHandles -> Test
test7 apiData = testCase "simulateUSDSell returns non-zero amount" $ do
    r <- simulateUSDSell (mtgoxDepthStoreHandle apiData) 1000
    case r of
        DepthStoreAnswer v -> assertBool "value is zero" (v /= 0)
        _ -> assertFailure "Simulation did not return any data."

test8 :: MtGoxAPIHandles -> Test
test8 apiData = testCase "simulateUSDBuy returns non-zero amount" $ do
    r <- simulateUSDBuy (mtgoxDepthStoreHandle apiData) 1000
    case r of
        DepthStoreAnswer v -> assertBool "value is zero" (v /= 0)
        _ -> assertFailure "Simulation did not return any data."

test9 :: MtGoxAPIHandles -> Test
test9 apiData = testCase "two simulations mostly match" $ do
    let startValue = 1000000
    m <- runMaybeT $ do
        usdValue <- MaybeT $ toMaybe <$> simulateBTCBuy
                                (mtgoxDepthStoreHandle apiData) startValue
        btcValue <- MaybeT $ toMaybe <$> simulateUSDSell
                                (mtgoxDepthStoreHandle apiData) usdValue
        let diff = abs (startValue - btcValue)
        liftIO $ assertBool "values differ more than 0.0001 BTC"
                    (diff <= 10000)
    case m of
        Nothing -> assertFailure "Simulation did not return any data."
        Just _ -> return ()
  where
    toMaybe (DepthStoreAnswer v) = Just v
    toMaybe _ = Nothing

test10 :: MtGoxAPIHandles -> Test
test10 apiData = testCase "another two simulations mostly match" $ do
    let startValue = 1000000
    m <- runMaybeT $ do
        usdValue <- MaybeT $ toMaybe <$> simulateBTCSell
                                (mtgoxDepthStoreHandle apiData) startValue
        btcValue <- MaybeT $ toMaybe <$> simulateUSDBuy
                                (mtgoxDepthStoreHandle apiData) usdValue
        let diff = abs (startValue - btcValue)
        liftIO $ assertBool "values differ more than 0.0001 BTC"
                    (diff <= 10000)
    case m of
        Nothing -> assertFailure "Simulation did not return any data."
        Just _ -> return ()
  where
    toMaybe (DepthStoreAnswer v) = Just v
    toMaybe _ = Nothing

mtgoxAPITests ::  [MtGoxAPIHandles -> Test]
mtgoxAPITests = [ test1, test2, test3, test4, test5
                , test6, test7, test8, test9, test10]

tests :: [Test]
tests = depthStoreTests

prepareAPI :: IO MtGoxAPIHandles
prepareAPI =
    let streamSettings =
            MtGoxStreamSettings DisableWalletNotifications SkipFullDepth
    in initMtGoxAPI (Just silentLogger) debugCredentials streamSettings

main :: IO ()
main = do
    apiData <- prepareAPI
    let apiTests = map ($ apiData) mtgoxAPITests
    defaultMain $ apiTests ++ tests

manualSubmitOrderTest :: IO ()
manualSubmitOrderTest = do
    apiData <- prepareAPI
    submitOrder apiData OrderTypeSellBTC cent >>= print
    submitOrder apiData OrderTypeBuyBTC cent >>= print
