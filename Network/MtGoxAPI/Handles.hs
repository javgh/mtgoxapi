module Network.MtGoxAPI.Handles
    ( MtGoxAPIHandles(..)
    ) where

import Network.MtGoxAPI.TickerMonitor
import Network.MtGoxAPI.DepthStore

data MtGoxAPIHandles = MtGoxAPIHandles
                        { mtgoxTickerMonitorHandle :: TickerMonitorHandle
                        , mtgoxDepthStoreHandle :: DepthStoreHandle
                        }
