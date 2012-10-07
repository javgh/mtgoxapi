module Network.MtGoxAPI
    ( initMtGoxAPI
    , getTickerStatus
    , simulateBTCSell
    , simulateBTCBuy
    , simulateUSDSell
    , waitForBTCDeposit
    , callHTTPApi
    , callHTTPApi'
    , module Network.MtGoxAPI.HttpAPI
    , module Network.MtGoxAPI.Credentials
    , module Network.MtGoxAPI.Handles
    , module Control.Watchdog
    , TickerStatus(..)
    , module Network.MtGoxAPI.Types
    ) where

import Control.Watchdog

import Network.MtGoxAPI.Credentials
import Network.MtGoxAPI.CurlWrapper
import Network.MtGoxAPI.DepthStore
import Network.MtGoxAPI.Handles
import Network.MtGoxAPI.HttpAPI
import Network.MtGoxAPI.StreamConnection
import Network.MtGoxAPI.TickerMonitor
import Network.MtGoxAPI.Types
import Network.MtGoxAPI.WalletNotifier

-- | Rolls all the individual init functions into one. Namely
-- 'initTickerMonitor', 'initDepthStore', 'initCurlWrapper',
-- 'initWalletNotifier' and finally 'initMtGoxStream'. All handles are returned
-- in one package and can then be used in combination with the various functions
-- from the submodules which are re-exported here for convenience.
initMtGoxAPI :: Maybe WatchdogLogger-> MtGoxCredentials -> FullDepthSetting -> IO MtGoxAPIHandles
initMtGoxAPI mLogger mtgoxCreds fullDepthSetting = do
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
    _ <- initMtGoxStream mtgoxCreds fullDepthSetting mtgoxAPIHandles
    return mtgoxAPIHandles

-- | Helper function to call functions from 'Network.MtGoxAPI.HttpAPI'.
callHTTPApi :: MtGoxAPIHandles-> (Maybe WatchdogLogger -> CurlHandle -> MtGoxCredentials -> t)-> t
callHTTPApi apiData f =
    f (mtgoxLogger apiData) (mtgoxCurlHandle apiData) (mtgoxCredentials apiData)

-- | Helper function to call functions from 'Network.MtGoxAPI.HttpAPI' that do
-- not use a logger.
callHTTPApi' :: MtGoxAPIHandles -> (CurlHandle -> MtGoxCredentials -> t) -> t
callHTTPApi' apiData f =
    f (mtgoxCurlHandle apiData) (mtgoxCredentials apiData)
