{-# LANGUAGE OverloadedStrings #-}
module Network.MtGoxAPI.StreamConnection
    ( initMtGoxStream
    , mtGoxTickerChannelUSD
    , mtGoxDepthChannelUSD
    , mtGoxTickerChannelNameEUR
    , mtGoxDepthChannelNameEUR
    , mtGoxTradeChannel
    , MtGoxStreamSettings(..)
    , WalletNotifierSetting(..)
    , FullDepthSetting(..)
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
import Network.MtGoxAPI.WalletNotifier

mtGoxStreamHost :: HostName
mtGoxStreamHost = "127.0.0.1"

mtGoxStreamPort :: PortID
mtGoxStreamPort = PortNumber 10508

mtGoxTradeChannel :: T.Text
mtGoxTradeChannel = "dbf1dee9-4f2e-4a08-8cb7-748919a71b21"

mtGoxTickerChannelUSD :: T.Text
mtGoxTickerChannelUSD = "d5f06780-30a8-4a48-a2f8-7ed181b4a13f"

mtGoxDepthChannelUSD :: T.Text
mtGoxDepthChannelUSD = "24e67e0d-1cad-4cc0-9e7a-f8523ef460fe"

mtGoxTickerChannelNameEUR :: T.Text
mtGoxTickerChannelNameEUR = "ticker.BTCEUR"

mtGoxDepthChannelNameEUR :: T.Text
mtGoxDepthChannelNameEUR = "depth.BTCEUR"

mtGoxTimeout :: Int
mtGoxTimeout = 2 * 60 * 10 ^ (6 :: Integer)

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

sendStreamCommand :: Handle -> MtGoxCredentials -> StreamCommand -> IO ()
sendStreamCommand h creds cmd = do
    encodedCmd <- encodeStreamCommand cmd creds
    BL.hPutStr h encodedCmd >> B.hPutStr h "\n"

openConnection :: HostName-> PortID-> MtGoxCredentials-> MtGoxStreamSettings-> MtGoxAPIHandles-> IO (Either String ())
openConnection host port creds streamSettings apiHandles = do
    status <- try go :: IO (Either IOException (Either String ()))
    case status of
        Right result -> return result
        Left ex -> return $ Left (show ex)
  where
    go = withSocketsDo $ do
        -- open connection
        h <- connectTo host port
        IO.hSetBuffering h IO.NoBuffering

        -- unsubscribe from default channels and subscribe to
        -- EUR channels
        mapM_ (sendStreamCommand h creds) $
            [ UnsubscribeCmd mtGoxTradeChannel
            , UnsubscribeCmd mtGoxTickerChannelUSD
            , UnsubscribeCmd mtGoxDepthChannelUSD
            , SubscribeCmd mtGoxTickerChannelNameEUR
            , SubscribeCmd mtGoxDepthChannelNameEUR
            ]

        -- wait for healthy activity to appear before proceeding
        _ <- waitForActivityWithTimeout h

        -- prepare handles
        let tickerMonitorHandle = mtgoxTickerMonitorHandle apiHandles
            depthStoreHandle = mtgoxDepthStoreHandle apiHandles
            walletNotifierHandle = mtgoxWalletNotifierHandle apiHandles

        -- extract extra settings
        let (MtGoxStreamSettings walletNotifierSetting fullDepthSetting) =
                streamSettings

        -- wallet notifier step
        when (walletNotifierSetting == EnableWalletNotifications) $ do
            -- get idkey and subscribe to wallet operations channel
            sendStreamCommand h creds IDKeyCmd
            (buffer1, idKey) <- waitForCallResultWithTimeout h parseIDKeyCallResult
            sendStreamCommand h creds $ PrivateSubscribeCmd (idkKey idKey)

            -- rewind buffer
            forM_ buffer1 $ \streamMessage -> do
                updateTickerStatus tickerMonitorHandle streamMessage
                updateDepthStoreFromMessage depthStoreHandle streamMessage
                updateWalletNotifier walletNotifierHandle streamMessage

        -- full depth step
        case fullDepthSetting of
            RequestFullDepth -> do
                -- get full depth
                sendStreamCommand h creds FullDepthCmd
                (buffer2, fullDepth) <- waitForCallResultWithTimeout h parseFullDepthCallResult
                updateDepthStoreFromFullDepth depthStoreHandle fullDepth

                -- rewind buffer
                forM_ buffer2 $ \streamMessage -> do
                    updateTickerStatus tickerMonitorHandle streamMessage
                    updateDepthStoreFromMessage depthStoreHandle streamMessage
                    updateWalletNotifier walletNotifierHandle streamMessage
            SkipFullDepth -> skipFullDepthRequest depthStoreHandle

        -- enter main loop and process incoming messages
        forever $ do
            streamMessage <- readNextStreamMessageWithTimeout h
            updateTickerStatus tickerMonitorHandle streamMessage
            updateDepthStoreFromMessage depthStoreHandle streamMessage
            updateWalletNotifier walletNotifierHandle streamMessage

waitForCallResultWithTimeout :: Handle -> (StreamMessage -> Maybe a) -> IO ([StreamMessage], a)
waitForCallResultWithTimeout h parser =
    wrapInTimeout h "Call result was not returned in time." (go [])
  where
    go buffer = do
        msg <- readNextStreamMessageWithTimeout h
        case msg of
            CallResult {} ->
                case parser msg of
                    Just result -> return (reverse buffer, result)
                    Nothing ->
                        throw $ userError "Unexpected call result was returned."
            other -> go (other:buffer)

waitForActivityWithTimeout :: Handle -> IO [StreamMessage]
waitForActivityWithTimeout h =
    wrapInTimeout h "Healthy activity did not appear in time." $ go []
  where
    go msgs = do
        msg <- readNextStreamMessageWithTimeout h
        let msgs' = msg : msgs
            checks = [ or (map isTickerUpdate msgs')
                     , or (map isDepthUpdate msgs')
                     ]
        if and checks
            then return $ reverse msgs'
            else go msgs'
    isTickerUpdate TickerUpdateUSD {} = True
    isTickerUpdate _ = False
    isDepthUpdate DepthUpdateUSD {} = True
    isDepthUpdate _ = False

-- | Starts a thread that will connect to the data stream
-- from Mt. Gox and supply the received data to the handles that are
-- are passed in. A watchdog maintains the connection.
initMtGoxStream :: MtGoxCredentials-> MtGoxStreamSettings -> MtGoxAPIHandles -> IO ThreadId
initMtGoxStream mtgoxCreds streamSettings mtgoxAPIHandles =
    let task = openConnection mtGoxStreamHost mtGoxStreamPort
                                    mtgoxCreds streamSettings
                                    mtgoxAPIHandles
        watchdogConf = do
            setResetDuration $ 90 * 10 ^ (6 :: Integer)
            case mtgoxLogger mtgoxAPIHandles of
                Just logger -> setLoggingAction logger
                Nothing -> return ()
            watch task
    in forkIO $ watchdog watchdogConf
