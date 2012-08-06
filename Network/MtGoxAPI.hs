module Network.MtGoxAPI
    (
    ) where

import Control.Concurrent
import Control.Monad

import Network.MtGoxAPI.Credentials
import Network.MtGoxAPI.DepthStore
import Network.MtGoxAPI.DepthStore.Monitor
import Network.MtGoxAPI.Handles
import Network.MtGoxAPI.StreamConnection
import Network.MtGoxAPI.TickerMonitor

initMtGoxAPI :: MtGoxCredentials -> IO MtGoxAPIHandles
initMtGoxAPI mtgoxCreds = do
    tickerMonitorHandle <- initTickerMonitor
    depthStoreHandle <- initDepthStore
    let mtgoxAPIHandles = MtGoxAPIHandles
                            { mtgoxTickerMonitorHandle = tickerMonitorHandle
                            , mtgoxDepthStoreHandle = depthStoreHandle
                            }
    initMtGoxStream mtgoxCreds mtgoxAPIHandles
    return mtgoxAPIHandles

main :: IO ()
main = do
    mtgoxHandles <- initMtGoxAPI debugCredentials
    let dsh = mtgoxDepthStoreHandle mtgoxHandles
        -- tmh = mtgoxTickerMonitorHandle mtgoxHandles
    monitorDepth dsh
--    forever $ do
--        getTickerStatus tmh >>= print
--        threadDelay (10 ^ (6 :: Integer))
