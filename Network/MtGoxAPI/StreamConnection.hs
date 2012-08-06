{-# LANGUAGE OverloadedStrings #-}
module Network.MtGoxAPI.StreamConnection
    ( initMtGoxStream
    , mtGoxTickerChannel
    , mtGoxDepthChannel
    , mtGoxTradeChannel
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Watchdog
import GHC.IO.Handle
import Network
import System.Timeout

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified System.IO as IO

import Network.MtGoxAPI.Credentials
import Network.MtGoxAPI.DepthStore.Adapter
import Network.MtGoxAPI.Handles
import Network.MtGoxAPI.StreamCommands
import Network.MtGoxAPI.TickerMonitor
import Network.MtGoxAPI.Types

mtGoxStreamHost = "127.0.0.1"
mtGoxStreamPort = PortNumber 10508

mtGoxTickerChannel :: T.Text
mtGoxTickerChannel = "d5f06780-30a8-4a48-a2f8-7ed181b4a13f"

mtGoxDepthChannel :: T.Text
mtGoxDepthChannel = "24e67e0d-1cad-4cc0-9e7a-f8523ef460fe"

mtGoxTradeChannel :: T.Text
mtGoxTradeChannel = "dbf1dee9-4f2e-4a08-8cb7-748919a71b21"

mtGoxTimeout :: Int
mtGoxTimeout = 2 * 60 * 10 ^ (6 :: Integer)

---- | Starts a thread that will connect to the data stream
---- from Mt. Gox and call the supplied hook setups once with a writer
---- for the connection and expects the setup to return the actual hook
---- which will then be called for every parsed message.
---- A watchdog maintains the connection.
--initMtGoxStream :: [HookSetup] -> IO ThreadId
--initMtGoxStream hookSetups =
--    let task = socketLoop hookSetups
--    in forkIO $ watchdog (watch task)

wrapInTimeout :: Handle -> String -> IO a -> IO a
wrapInTimeout h errMsg action = do
    resultM <- timeout mtGoxTimeout action
    case resultM of
        Just result -> return result
        Nothing -> do
            hClose h
            throw $ userError errMsg

readNextStreamMessageWithTimeout :: Handle -> IO StreamMessage
readNextStreamMessageWithTimeout h =
    parseStreamLine <$>
        wrapInTimeout h "MtGox did not send data for a while." (B.hGetLine h)

sendStreamCommand h creds cmd = do
    encodedCmd <- encodeStreamCommand cmd creds
    BL.hPutStr h encodedCmd >> B.hPutStr h "\n"

openConnection host port creds apiHandles = do
    status <- try go :: IO (Either IOException (Either String ()))
    case status of
        Right result -> return result
        Left ex -> return $ Left (show ex)
  where
    go = withSocketsDo $ do
        -- open connection
        h <- connectTo host port
        IO.hSetBuffering h IO.NoBuffering

        -- wait for default subscribe messages; then unsubscribe
        -- from trade channel
        _ <- waitForSubscribesWithTimeout h
        sendStreamCommand h creds $ UnsubscribeCmd mtGoxTradeChannel

        -- get idkey and subscribe to wallet operations channel
        sendStreamCommand h creds IDKeyCmd
        idKey <- waitForCallResultWithTimeout h parseIDKeyCallResult
        sendStreamCommand h creds $ PrivateSubscribeCmd (idkKey idKey)

        -- prepare handles
        let tickerMonitorHandle = mtgoxTickerMonitorHandle apiHandles
            depthStoreHandle = mtgoxDepthStoreHandle apiHandles

        -- get full depth
        sendStreamCommand h creds FullDepthCmd
        fullDepth <- waitForCallResultWithTimeout h parseFullDepthCallResult
        updateDepthStoreFromFullDepth depthStoreHandle fullDepth

        -- enter main loop and process incoming messages
        forever $ do
            streamMessage <- readNextStreamMessageWithTimeout h
            updateTickerStatus tickerMonitorHandle streamMessage
            updateDepthStoreFromMessage depthStoreHandle streamMessage

waitForCallResultWithTimeout :: Handle -> (StreamMessage -> Maybe a) -> IO a
waitForCallResultWithTimeout h parser =
    wrapInTimeout h "Call result was not returned in time." go
  where
    go = do
        msg <- readNextStreamMessageWithTimeout h
        case msg of
            CallResult {} ->
                case parser msg of
                    Just result -> return result
                    Nothing ->
                        throw $ userError "Unexpected call result was returned."
            _ -> go

waitForSubscribesWithTimeout :: Handle -> IO [StreamMessage]
waitForSubscribesWithTimeout h =
    wrapInTimeout h "Subscribe messages did not appear in time." $ go []
  where
    go msgs = do
        msg <- readNextStreamMessageWithTimeout h
        let msgs' = msg : msgs
            checks = [ Subscribed mtGoxTickerChannel `elem` msgs'
                     , Subscribed mtGoxDepthChannel `elem` msgs'
                     , Subscribed mtGoxTradeChannel `elem` msgs'
                     ]
        if and checks
            then return $ reverse msgs'
            else go msgs'

initMtGoxStream mtgoxCreds mtgoxAPIHandles =
    let task = openConnection mtGoxStreamHost mtGoxStreamPort
                                    mtgoxCreds mtgoxAPIHandles
        ninetySeconds = 90 * 10 ^ (6 :: Integer)
    in forkIO $ watchdog (setResetDuration ninetySeconds >> watch task)
