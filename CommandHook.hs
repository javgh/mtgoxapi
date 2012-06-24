{-# LANGUAGE OverloadedStrings #-}
module CommandHook
    ( initCommandHook
    , sendPrivateInfoCmd
    ) where

import Control.Applicative
import Control.Concurrent
import Data.Aeson.Types

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

-- TODO: how to have watchChannel wait until writer is ready?

-- TODO: maybe wrap the channel in a newtype (do research) to
-- have cleaner exported functionality

-- TODO: add timeout, in case no reply is received
-- see System.Timeout
--
-- long TODO: replay cmds in case they haven't been executed,
-- but before doing that, double-check whether they were really
-- not executed

initCommandHook :: IO (Chan CommandHookMsg, HookSetup)
initCommandHook = do
    chan <- newChan :: IO (Chan CommandHookMsg)
    writerStore <- newMVar (Nothing :: Maybe StreamWriter)
    _ <- forkIO $ watchChannel chan writerStore
    return (chan, commandHookSetup chan writerStore)

commandHookSetup chan writerStore writer = do
    _ <- swapMVar writerStore (Just writer)
    return $ commandHook chan

commandHook :: Chan CommandHookMsg -> StreamMessage -> IO ()
commandHook chan msg = writeChan chan $ NewMessage msg

watchChannel chan writerStore = go M.empty
  where
    go outstandingReplies = do
        msg <- readChan chan
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

sendPrivateInfoCmd :: Chan CommandHookMsg -> IO PrivateInfoReply
sendPrivateInfoCmd chan = do
    answerChan <- newChan :: IO (Chan Value)
    let cmd = PrivateInfo ""
    writeChan chan $ SendCommand cmd answerChan
    answer <- parsePrivateInfoReply <$> readChan answerChan
    case answer of
        Success answer' -> return answer'
        Error _ -> undefined
