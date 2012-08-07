module Network.MtGoxAPI
    (
    ) where

import Control.Concurrent
import Control.Monad
import Control.Watchdog

import Network.MtGoxAPI.Credentials
import Network.MtGoxAPI.CurlWrapper
import Network.MtGoxAPI.DepthStore
import Network.MtGoxAPI.DepthStore.Monitor
import Network.MtGoxAPI.Handles
import Network.MtGoxAPI.HttpAPI
import Network.MtGoxAPI.StreamConnection
import Network.MtGoxAPI.TickerMonitor
import Network.MtGoxAPI.WalletNotifier

initMtGoxAPI :: Maybe WatchdogLogger -> MtGoxCredentials -> IO MtGoxAPIHandles
initMtGoxAPI mLogger mtgoxCreds = do
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
    initMtGoxStream mtgoxCreds mtgoxAPIHandles
    return mtgoxAPIHandles

-- | Helper function to call
callHTTPApi :: MtGoxAPIHandles-> (Maybe WatchdogLogger -> CurlHandle -> MtGoxCredentials -> t)-> t
callHTTPApi apiData f =
    f (mtgoxLogger apiData) (mtgoxCurlHandle apiData) (mtgoxCredentials apiData)

main :: IO ()
main = do
    mtgoxHandles <- initMtGoxAPI Nothing debugCredentials

    callHTTPApi mtgoxHandles getPrivateInfoR >>= print
    putStrLn "waiting for deposit"
    waitForBTCDeposit (mtgoxWalletNotifierHandle mtgoxHandles)
    putStrLn "seen deposit"

    --let dsh = mtgoxDepthStoreHandle mtgoxHandles
    --    -- tmh = mtgoxTickerMonitorHandle mtgoxHandles
    --monitorDepth dsh
--    forever $ do
--        getTickerStatus tmh >>= print
--        threadDelay (10 ^ (6 :: Integer))
