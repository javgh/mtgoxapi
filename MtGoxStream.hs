{-# LANGUAGE OverloadedStrings #-}
module MtGoxStream
    ( initMtGoxStream
    , mtGoxTickerChannel
    , mtGoxDepthChannel
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Watchdog
import GHC.IO.Handle
import Network

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified System.IO as IO

import HookUtils
import StreamCommand
import StreamParsing

mtGoxStreamHost = "127.0.0.1"
mtGoxStreamPort = PortNumber 10508

mtGoxTickerChannel :: T.Text
mtGoxTickerChannel = "d5f06780-30a8-4a48-a2f8-7ed181b4a13f"

mtGoxDepthChannel :: T.Text
mtGoxDepthChannel = "24e67e0d-1cad-4cc0-9e7a-f8523ef460fe"

socketLoop :: [HookSetup]-> IO (Either String ())
socketLoop hookSetups = do
    status <- try go :: IO (Either IOException (Either String ()))
    case status of
        Right result -> return result
        Left ex -> return $ Left (show ex)
  where
    go = withSocketsDo $ do
        h <- connectTo mtGoxStreamHost mtGoxStreamPort
        IO.hSetBuffering h IO.NoBuffering
        hooks <- mapM (\hookSetup -> hookSetup (writer h)) hookSetups
        forever $ do
            streamMessage <- parseStreamLine <$> B.hGetLine h
            mapM_ (\hook -> hook streamMessage) hooks

writer :: Handle -> StreamCommand -> IO ()
writer h cmd = do
    _ <- try go :: IO (Either IOException ())
    return ()   -- ignore any exceptions; they should hopefully soon
                -- be caught on socketLoop's thread as well
  where
    go = BL.hPutStr h (encodeStreamCommand cmd) >> BL.hPutStr h "\n"

-- | Starts a thread that will connect to the data stream
-- from Mt. Gox and call the supplied hook setups once with a writer
-- for the connection and expects the setup to return the actual hook
-- which will then be called for every parsed message.
-- A watchdog maintains the connection.
initMtGoxStream :: [HookSetup] -> IO ThreadId
initMtGoxStream hookSetups = do
    let watchdogConf = defaultWatchdogConfig
        task = socketLoop hookSetups
    forkIO $ watchdog watchdogConf (timeTask task)
