{-# LANGUAGE OverloadedStrings #-}
module TickerMonitor
    ( initTickerMonitor
    , TickerStatus(..)
    ) where

import Control.Concurrent
import Control.Watchdog
import Data.Time.Clock

import HookUtils
import StreamCommand
import StreamParsing

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

getTickerStatus :: MVar (Maybe TickerStatus) -> IO TickerStatus
getTickerStatus store =
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

tickerStreamMessageHookSetup :: MVar (Maybe TickerStatus) -> StreamWriter -> IO Hook
tickerStreamMessageHookSetup tickerStore _ = return $ tickerStreamMessageHook tickerStore

tickerStreamMessageHook :: MVar (Maybe TickerStatus) -> StreamMessage -> IO ()
tickerStreamMessageHook tickerStore (update@TickerUpdateUSD {}) = do
    now <- getCurrentTime
    let tickerStatus = TickerStatus { tsTimestamp = now
                                    , tsBid = tuBid update
                                    , tsAsk = tuAsk update
                                    , tsLast = tuLast update
                                    , tsPrecision = expectedPrecision
                                    }
    _ <- swapMVar tickerStore (Just tickerStatus)
    return ()
tickerStreamMessageHook _ _ = return ()


-- | Will start a thread that monitors Mt.Gox's ticker
-- and will return a function that can be used to access
-- the latest ticker status in a safe way (it will ensure
-- that fresh data is returned).

-- | Returns two things:
--
--   * a hook, which can be used to scan stream messages for ticker updates
--
--   * a function that can be used to access the latest ticker status in a safe way
--     (it will ensure that fresh data is returned).
--
initTickerMonitor :: IO (HookSetup, IO TickerStatus)
initTickerMonitor = do
    store <- newMVar Nothing
    let hookSetup = tickerStreamMessageHookSetup store
        f = getTickerStatus store
    return (hookSetup, f)
