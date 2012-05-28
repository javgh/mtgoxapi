{-# LANGUAGE OverloadedStrings #-}
module StreamMonitor where

import Control.Concurrent
import Control.Monad

import AuthCommand
import AuthCommandReplyParsing
import ChannelJoiner
import MtGoxStream
import StreamCommand
import StreamParsing

debugHookSetup writer = do
    forkIO $ do
        threadDelay 3000000
        nonce <- getNonce
        writer PrivateInfo { piNonce = nonce }
    return debugHook

debugHook (CallResult { crResult = crResult }) = print $ parseAuthCommandReply crResult
debugHook msg = print msg

main :: IO ()
main = do
    tid <- initMtGoxStream [debugHookSetup, channelJoinerHookSetup []]
    threadDelay 1000000

    forever $ threadDelay 1000000
