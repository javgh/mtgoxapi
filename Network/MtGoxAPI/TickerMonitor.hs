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

-- | Access latest ticker status in a safe way (it will be checked, whether
-- fresh data is available).
getTickerStatus :: TickerMonitorHandle -> IO TickerStatus
getTickerStatus TickerMonitorHandle { unTMH = store } =
    let task = getTickerStatus' store
    in watchdog $ do
        setInitialDelay 200000 {- 200 ms -}
        setMaximumDelay 200000000 {- 20 seconds -}
        setLoggingAction silentLogger
        watch task

getTickerStatus' :: MVar (Maybe TickerStatus) -> IO (Either String TickerStatus)
getTickerStatus' store = do
    tickerStatusM <- readMVar store
    case tickerStatusM of
        Nothing -> return $ Left "No ticker data present"
        Just tickerStatus -> do
            now <- getCurrentTime
            let age = diffUTCTime now (tsTimestamp tickerStatus)
            return $ decideOnAge age tickerStatus
  where
    decideOnAge age tickerStatus
      | age < 60 = Right tickerStatus
      | age >= 60 && age <= 300 = Left "Data stale"    -- will retry
      | otherwise = Right TickerUnavailable            -- give up

-- | Update ticker with new data
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
