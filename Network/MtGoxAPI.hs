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

-- | Helper function to call functions from 'Network.MtGoxAPI.HttpAPI'.
callHTTPApi :: MtGoxAPIHandles-> (Maybe WatchdogLogger -> CurlHandle -> MtGoxCredentials -> t)-> t
callHTTPApi apiData f =
    f (mtgoxLogger apiData) (mtgoxCurlHandle apiData) (mtgoxCredentials apiData)

-- | Helper function to call functions from 'Network.MtGoxAPI.HttpAPI' that do
-- not use a logger.
callHTTPApi' :: MtGoxAPIHandles -> (CurlHandle -> MtGoxCredentials -> t) -> t
callHTTPApi' apiData f =
    f (mtgoxCurlHandle apiData) (mtgoxCredentials apiData)
