-- |
-- This library offers a wrapper around the Mt.Gox API. It focuses, above all,
-- on reliability, in that many actions - if they are idempotent - are
-- automatically retried for some time in case of errors (using
-- 'Control.Watchdog').
--
-- The library is able to maintain a copy of the current Mt.Gox order book by
-- subscribing to real-time updates via the Mt.Gox websocket API. Based on this
-- data the cost of hypothetical orders can then be calculated (e.g.
-- 'simulateBTCBuy'). For this feature to work, it is necessary to run an
-- instance of the MtGoxCachingProxy (
-- https://github.com/javgh/MtGoxCachingProxy ) to which the library will
-- connect (using port 10508 on localhost).
--
-- NOTE: The library is currently hardcoded to work on the BTC/EUR pair for
-- Mt.Gox only. This was done in a very hackish way, which is why many of the
-- function names still contain 'USD', because BTC/USD was the previously used
-- pair. I apologize for this terrible source of confusion. I had planned to
-- clean this up, but you know how priorities change around sometimes. Patches
-- to fix this properly, at the cost of breaking changes, and to ideally make the
-- currency pair configurable are welcome.
--
-- Example usage:
--
-- > import Network.MtGoxAPI
-- >
-- > import qualified Data.ByteString.Char8 as B8
-- >
-- > main :: IO ()
-- > main = do
-- >     putStrLn "Please provide your API key: "
-- >     authKey <- getLine
-- >     putStrLn "Please provide your API secret: "
-- >     authSecret <- getLine
-- >     let credentials = initMtGoxCredentials (B8.pack authKey)
-- >                                            (B8.pack authSecret)
-- >         streamSettings = MtGoxStreamSettings DisableWalletNotifications
-- >                                              SkipFullDepth
-- >     apiHandles <- initMtGoxAPI Nothing credentials streamSettings
-- >     getPrivateInfoR apiHandles >>= print
--
-- Example output:
--
-- @
-- Please provide your API key:
-- ...
-- Please provide your API secret:
-- ...
-- Right (PrivateInfo {piBtcBalance = 1000000, piUsdBalance = 846450,
-- piBtcOperations = 441, piUsdOperations = 6, piFee = 0.45})
-- @
--

module Network.MtGoxAPI
    ( initMtGoxAPI
    , getTickerStatus
    , simulateBTCSell
    , simulateBTCBuy
    , simulateUSDSell
    , simulateUSDBuy
    , waitForBTCDeposit
    , getOrderCountR
    , getPrivateInfoR
    , getBitcoinDepositAddressR
    , withdrawBitcoins
    , submitOrder
    , submitBtcBuyOrder
    , submitBtcSellOrder
    , module Network.MtGoxAPI.Credentials
    , module Network.MtGoxAPI.Handles
    , module Control.Watchdog
    , TickerStatus(..)
    , DepthStoreAnswer(..)
    , H.OrderStats(..)
    , module Network.MtGoxAPI.Types
    ) where

import Control.Watchdog

import Network.MtGoxAPI.Credentials
import Network.MtGoxAPI.CurlWrapper
import Network.MtGoxAPI.DepthStore
import Network.MtGoxAPI.Handles
import Network.MtGoxAPI.StreamConnection
import Network.MtGoxAPI.TickerMonitor
import Network.MtGoxAPI.Types
import Network.MtGoxAPI.WalletNotifier

import qualified Network.MtGoxAPI.HttpAPI as H

-- | Rolls all the individual init functions into one. Namely
-- 'initTickerMonitor', 'initDepthStore', 'initCurlWrapper',
-- 'initWalletNotifier' and finally 'initMtGoxStream'. All handles are returned
-- in one package and can then be used in combination with the various functions
-- from the submodules which are re-exported here for convenience.
initMtGoxAPI :: Maybe WatchdogLogger-> MtGoxCredentials -> MtGoxStreamSettings -> IO MtGoxAPIHandles
initMtGoxAPI mLogger mtgoxCreds mtgoxStreamSettings = do
    tickerMonitorHandle <- initTickerMonitor
    depthStoreHandle <- initDepthStore
    curlHandle <- initCurlWrapper
    walletNotifierHandle <- initWalletNotifier
    let mtgoxAPIHandles = MtGoxAPIHandles
                            { mtgoxCredentials = mtgoxCreds
                            , mtgoxLogger = mLogger
                            , mtgoxCurlHandle = curlHandle
                            , mtgoxTickerMonitorHandle = tickerMonitorHandle
                            , mtgoxDepthStoreHandle = depthStoreHandle
                            , mtgoxWalletNotifierHandle = walletNotifierHandle
                            }
    _ <- initMtGoxStream mtgoxCreds mtgoxStreamSettings mtgoxAPIHandles
    return mtgoxAPIHandles

-- | Wrapper around 'H.getOrderCountR'
getOrderCountR :: MtGoxAPIHandles -> IO (Either String OpenOrderCount)
getOrderCountR apiData = callHTTPApi apiData H.getOrderCountR

-- | Wrapper around 'H.getPrivateInfoR'
getPrivateInfoR :: MtGoxAPIHandles -> IO (Either String PrivateInfo)
getPrivateInfoR apiData = callHTTPApi apiData H.getPrivateInfoR

-- | Wrapper around 'H.getBitcoinDepositAddressR'
getBitcoinDepositAddressR :: MtGoxAPIHandles -> IO (Either String BitcoinDepositAddress)
getBitcoinDepositAddressR apiData = callHTTPApi apiData H.getBitcoinDepositAddressR

-- | Wrapper around 'H.withdrawBitcoins'
withdrawBitcoins :: MtGoxAPIHandles-> BitcoinAddress -> Integer -> IO (Either String WithdrawResult)
withdrawBitcoins apiData = callHTTPApi' apiData H.withdrawBitcoins

-- | Wrapper around 'H.submitOrder'
submitOrder :: MtGoxAPIHandles-> OrderType -> Integer -> IO (Either String H.OrderStats)
submitOrder apiData = callHTTPApi apiData H.submitOrder

-- | Wrapper around 'H.submitBtcBuyOrder'
submitBtcBuyOrder :: MtGoxAPIHandles -> Integer -> IO (Either String Order)
submitBtcBuyOrder apiData = callHTTPApi' apiData H.submitBtcBuyOrder

-- | Wrapper around 'H.submitBtcSellOrder'
submitBtcSellOrder :: MtGoxAPIHandles -> Integer -> IO (Either String Order)
submitBtcSellOrder apiData = callHTTPApi' apiData H.submitBtcSellOrder

-- | Helper function to call functions from 'Network.MtGoxAPI.HttpAPI'.
callHTTPApi :: MtGoxAPIHandles-> (Maybe WatchdogLogger -> CurlHandle -> MtGoxCredentials -> t)-> t
callHTTPApi apiData f =
    f (mtgoxLogger apiData) (mtgoxCurlHandle apiData) (mtgoxCredentials apiData)

-- | Helper function to call functions from 'Network.MtGoxAPI.HttpAPI' that do
-- not use a logger.
callHTTPApi' :: MtGoxAPIHandles -> (CurlHandle -> MtGoxCredentials -> t) -> t
callHTTPApi' apiData f =
    f (mtgoxCurlHandle apiData) (mtgoxCredentials apiData)
