module Network.MtGoxAPI.Handles
    ( MtGoxAPIHandles(..)
    ) where

import Control.Watchdog

import Network.MtGoxAPI.Credentials
import Network.MtGoxAPI.CurlWrapper
import Network.MtGoxAPI.DepthStore
import Network.MtGoxAPI.TickerMonitor

data MtGoxAPIHandles = MtGoxAPIHandles
                        { mtgoxCredentials :: MtGoxCredentials
                        , mtgoxLogger :: Maybe WatchdogLogger
                        , mtgoxTickerMonitorHandle :: TickerMonitorHandle
                        , mtgoxDepthStoreHandle :: DepthStoreHandle
                        , mtgoxCurlHandle :: CurlHandle
                        }
