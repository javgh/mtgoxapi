module Network.MtGoxAPI.Handles
    ( MtGoxAPIHandles(..)
    ) where

import Control.Watchdog

import Network.MtGoxAPI.Credentials
import Network.MtGoxAPI.CurlWrapper
import Network.MtGoxAPI.DepthStore
import Network.MtGoxAPI.TickerMonitor
import Network.MtGoxAPI.WalletNotifier

data MtGoxAPIHandles = MtGoxAPIHandles
                        { mtgoxCredentials :: MtGoxCredentials
                        , mtgoxLogger :: Maybe WatchdogLogger
                        , mtgoxCurlHandle :: CurlHandle
                        , mtgoxTickerMonitorHandle :: TickerMonitorHandle
                        , mtgoxDepthStoreHandle :: DepthStoreHandle
                        , mtgoxWalletNotifierHandle :: WalletNotifierHandle
                        }
