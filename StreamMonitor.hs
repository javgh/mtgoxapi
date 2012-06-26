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

debugHookSetup writer = do
    forkIO $ do
        threadDelay 8000000
        nonce <- getNonce
        --let order = Order { scNonce = nonce
        --                  , scOrderType = OrderTypeSellBTC
        --                  , scAmount = round (0.01 * 10^8)
        --                  }
        --let key = "xjmVIDKcR0KEeQZiWsuPxgAAAABP6wV/QO6Iarjpdcrr54/KczeymoJEJft2iE1SU4VKnPCXHkQ"
        --writer $ StreamCommandNoReply (PrivateSubscribeCmd key)
        let withdraw = WithdrawBTC { scNonce = nonce
                                   , scAddress = "1BidUjjWMQMk69XiKE5J3YUZaJmx23EfPW"
                                   , scAmount = round (0.01 * 10^8)
                                   }
        writer $ StreamCommandWithReply withdraw
    return debugHook

--debugHook (CallResult { crResult = crResult }) = print $ parseFullDepthReply crResult
debugHook (CallResult { crResult = crResult }) = print crResult
debugHook msg = print msg

main :: IO ()
main = do
    tid <- initMtGoxStream [debugHookSetup, channelJoinerHookSetup ["c6399520-329c-4742-8479-06625acb8fc6"]]
    --(commandHookChan, commandHookSetup) <- initCommandHook
    --tid <- initMtGoxStream [ commandHookSetup , channelJoinerHookSetup []]

--    forever $ do
--        line <- getLine
--        case line of
--            "b" -> do
--                reply <- sendOrderCmd commandHookChan OrderTypeBuyBTC (round (0.01 * 10^8))
--                print reply
--            "s" -> do
--                reply <- sendOrderCmd commandHookChan OrderTypeSellBTC (round (0.01 * 10^8))
--                print reply
--            _ -> return ()

--    threadDelay 8000000
--
--    sendOpenOrderCountCmd commandHookChan >>= print

    forever $ threadDelay 1000000
