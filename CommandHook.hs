{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module CommandHook
    ( CommandHookChan
    , initCommandHook
    , sendPrivateInfoCmd
    , sendFullDepthCmd
    ) where

import Control.Applicative
import Control.Concurrent
import Data.Aeson.Types
import System.Timeout

import qualified Data.Map as M

import AuthCommand
import AuthCommandReplyParsing
import HookUtils
import StreamCommand
import StreamParsing

data CommandHookMsg = SendCommand { chmCommand :: StreamCommandWR
                                  , chmAnswerChannel :: Chan Value
                                  }
                      | NewMessage { chmMessage :: StreamMessage }

newtype CommandHookChan = CommandHookChan { chcChan :: Chan CommandHookMsg }

initCommandHook :: IO (CommandHookChan, HookSetup)
initCommandHook = do
    isReadyChan <- newChan :: IO (Chan ())
    cmdChan <- newChan :: IO (Chan CommandHookMsg)
    writerStore <- newMVar (Nothing :: Maybe StreamWriter)
    _ <- forkIO $ watchChannel isReadyChan cmdChan writerStore
    return (CommandHookChan cmdChan, commandHookSetup isReadyChan cmdChan writerStore)

commandHookSetup :: Chan ()-> Chan CommandHookMsg -> MVar (Maybe a)-> a -> IO (StreamMessage -> IO ())
commandHookSetup isReadyChan cmdChan writerStore writer = do
    _ <- swapMVar writerStore (Just writer)
    writeChan isReadyChan ()
    return $ commandHook cmdChan

commandHook :: Chan CommandHookMsg -> StreamMessage -> IO ()
commandHook cmdChan msg = writeChan cmdChan $ NewMessage msg

watchChannel :: Chan () -> Chan CommandHookMsg -> MVar (Maybe (StreamCommand -> IO a))-> IO b
watchChannel isReadyChan cmdChan writerStore = do
    _ <- readChan isReadyChan
    go M.empty
  where
    go !outstandingReplies = do
        msg <- readChan cmdChan
        case msg of
            SendCommand { chmCommand = cmd, chmAnswerChannel = answerChan } -> do
                nonce <- getNonce
                let cmd' = cmd { scNonce = nonce }
                Just writer <- readMVar writerStore
                writer $ StreamCommandWithReply cmd'
                go $ M.insert nonce answerChan outstandingReplies
            NewMessage { chmMessage = msg } -> case msg of
                CallResult { crID = nonce, crResult = result } ->
                    case M.lookup nonce outstandingReplies of
                        Just answerChan -> do
                            writeChan answerChan result
                            go $ M.delete nonce outstandingReplies
                        Nothing -> go outstandingReplies
                _ -> go outstandingReplies

sendCmd :: Chan CommandHookMsg -> StreamCommandWR -> IO (Maybe Value)
sendCmd cmdChan cmd = do
    answerChan <- newChan :: IO (Chan Value)
    writeChan cmdChan $ SendCommand cmd answerChan
    timeout (30 * 10^6) $ readChan answerChan

-- | Executes command and waits for reply with a timeout of 30 seconds.
sendPrivateInfoCmd :: CommandHookChan -> IO (Maybe PrivateInfoReply)
sendPrivateInfoCmd (CommandHookChan cmdChan) = do
    answer <- sendCmd cmdChan $ PrivateInfo ""
    return $ answer >>= parsePrivateInfoReply

sendFullDepthCmd :: CommandHookChan -> IO (Maybe FullDepthReply)
sendFullDepthCmd (CommandHookChan cmdChan) = do
    answer <- sendCmd cmdChan $ FullDepth ""
    return $ answer >>= parseFullDepthReply
