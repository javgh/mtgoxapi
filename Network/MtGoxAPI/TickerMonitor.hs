module Network.MtGoxAPI.TickerMonitor
    ( initTickerMonitor
    , getTickerStatus
    , updateTickerStatus
    , TickerStatus(..)
    , TickerMonitorHandle
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Watchdog
import Data.Time.Clock

import Network.MtGoxAPI.Types

newtype TickerMonitorHandle = TickerMonitorHandle
                                { unTMH :: MVar (Maybe TickerStatus) }

data TickerStatus = TickerStatus { tsTimestamp :: UTCTime
                                 , tsBid :: Integer
                                 , tsAsk :: Integer
                                 , tsLast :: Integer
                                 , tsPrecision :: Integer
                                 }
                    | TickerUnavailable
                    deriving (Show)

expectedPrecision :: Integer
expectedPrecision = 5

maximumAgeInSeconds :: NominalDiffTime
maximumAgeInSeconds = 300

watchdogSettings :: WatchdogAction ()
watchdogSettings = do
    setLoggingAction silentLogger
    setInitialDelay 250000    -- 250 ms
    setMaximumRetries 6
    -- will fail after:
    -- 0.25 + 0.5 + 1 + 2 + 4 + 8 seconds = 15.75 seconds

-- | Access latest ticker status in a safe way. It will be checked, whether
-- fresh data is available (never older than 300 seconds) and if not, re-try a
-- few times before giving up. The function will not block for longer than
-- about 20 seconds.
getTickerStatus :: TickerMonitorHandle -> IO TickerStatus
getTickerStatus TickerMonitorHandle { unTMH = store } = do
    let task = getTickerStatus' store
    result <- watchdog $ do
                watchdogSettings
                watchImpatiently task
    return $ case result of
                Left _ -> TickerUnavailable
                Right status -> status

getTickerStatus' :: MVar (Maybe TickerStatus) -> IO (Either String TickerStatus)
getTickerStatus' store = do
    tickerStatusM <- readMVar store
    case tickerStatusM of
        Nothing -> return $ Left "No ticker data present"
        Just tickerStatus -> do
            now <- getCurrentTime
            let age = diffUTCTime now (tsTimestamp tickerStatus)
            return $ if age < maximumAgeInSeconds
                        then Right tickerStatus
                        else Left "Data stale"

-- | Update ticker with new data.
updateTickerStatus :: TickerMonitorHandle -> StreamMessage -> IO ()
updateTickerStatus TickerMonitorHandle { unTMH = tickerStore }
                   (update@TickerUpdateUSD {}) = do
    now <- getCurrentTime
    let tickerStatus = TickerStatus { tsTimestamp = now
                                    , tsBid = tuBid update
                                    , tsAsk = tuAsk update
                                    , tsLast = tuLast update
                                    , tsPrecision = expectedPrecision
                                    }
    _ <- swapMVar tickerStore (Just tickerStatus)
    return ()
updateTickerStatus _ _ = return ()

-- | Initializes ticker monitor and returns a handle for it.
initTickerMonitor :: IO TickerMonitorHandle
initTickerMonitor = TickerMonitorHandle <$> newMVar Nothing
