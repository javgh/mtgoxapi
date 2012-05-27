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

import StreamParsing
import StreamCommand

mtGoxStreamHost = "127.0.0.1"
mtGoxStreamPort = PortNumber 10508

mtGoxTickerChannel :: T.Text
mtGoxTickerChannel = "d5f06780-30a8-4a48-a2f8-7ed181b4a13f"

mtGoxDepthChannel :: T.Text
mtGoxDepthChannel = "24e67e0d-1cad-4cc0-9e7a-f8523ef460fe"

-- TODO: figure out right moment to send these
--initialCmds =
--    [ "{\"op\":\"unsubscribe\", \"channel\":\"dbf1dee9-4f2e-4a08-8cb7-748919a71b21\"}\n"
--    , "{\"op\":\"unsubscribe\", \"channel\":\"24e67e0d-1cad-4cc0-9e7a-f8523ef460fe\"}\n"
--    , "{\"op\":\"unsubscribe\", \"channel\":\"d5f06780-30a8-4a48-a2f8-7ed181b4a13f\"}\n"
--    ]

socketLoop :: [StreamMessage -> StreamWriter -> IO ()]-> IO (Either String ())
socketLoop hooks = do
    status <- try go :: IO (Either IOException (Either String ()))
    case status of
        Right result -> return result
        Left ex -> return $ Left (show ex)
  where
    go = withSocketsDo $ do
        h <- connectTo mtGoxStreamHost mtGoxStreamPort
        IO.hSetBuffering h IO.NoBuffering
        forever $ do
            streamMessage <- parseStreamLine <$> B.hGetLine h
            mapM_ (\hook -> hook streamMessage (writer h)) hooks

writer :: Handle -> StreamCommand -> IO ()
writer h cmd = BL.hPutStr h (encodeStreamCommand cmd) >> BL.hPutStr h "\n"

-- | Starts a thread that will connect to the data stream
-- from Mt. Gox and call the supplied hooks with parsed messages.
-- A watchdog maintains the connection.
initMtGoxStream :: [StreamMessage -> StreamWriter -> IO ()] -> IO ThreadId
initMtGoxStream hooks = do
    let watchdogConf = defaultWatchdogConfig
        task = socketLoop hooks
    forkIO $ watchdog watchdogConf (timeTask task)
