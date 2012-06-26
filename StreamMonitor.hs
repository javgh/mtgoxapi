{-# LANGUAGE OverloadedStrings #-}
module StreamMonitor where

import Control.Concurrent
import Control.Monad

import AuthCommand
import AuthCommandReplyParsing
import ChannelJoiner
import CommandHook
import DepthStore
import DepthStoreAdapter
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
    (depthStoreChan, depthStoreHookSetup) <- initDepthStoreAdapter commandHookChan
    --tid <- initMtGoxStream [debugHookSetup, channelJoinerHookSetup []]
    tid <- initMtGoxStream [ commandHookSetup
                           , channelJoinerHookSetup [mtGoxDepthChannel]
                           , depthStoreHookSetup
                           ]
    threadDelay 8000000
    forever $ do
        --p1 <- simulateBTCSell depthStoreChan (10 * 10^8)
        --p2 <- simulateBTCBuy depthStoreChan (10 * 10^8)
        --putStrLn $ "Sell 10 BTC: " ++ show p1 ++ "\t\tBuy 10 BTC: " ++ show p2
        --when (p1 > p2) $ error "Inconsistency! Selling makes more money than buying costs" threadDelay 1000000
        threadDelay 1000000
