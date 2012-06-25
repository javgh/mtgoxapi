{-# LANGUAGE OverloadedStrings #-}
module StreamMonitor where

import Control.Concurrent
import Control.Monad

import AuthCommand
import AuthCommandReplyParsing
import ChannelJoiner
import CommandHook
import MtGoxStream
import StreamCommand
import StreamParsing

--debugHookSetup writer = do
--    forkIO $ do
--        threadDelay 5000000
--        nonce <- getNonce
--        writer FullDepth { scNonce = nonce }
--    return debugHook
--
--debugHook (CallResult { crResult = crResult }) = print $ parseFullDepthReply crResult
----debugHook (CallResult { crResult = crResult }) = print crResult
--debugHook msg = print msg

main :: IO ()
main = do
    (commandHookChan, commandHookSetup) <- initCommandHook
    --tid <- initMtGoxStream [debugHookSetup, channelJoinerHookSetup []]
    tid <- initMtGoxStream [commandHookSetup, channelJoinerHookSetup []]
    threadDelay 5000000
    cmdResult <- sendFullDepthCmd commandHookChan
    print cmdResult
    forever $ threadDelay 1000000
