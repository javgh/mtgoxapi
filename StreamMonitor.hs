{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Monad

import ChannelJoiner
import MtGoxStream

debugHookSetup _ = return $ debugHook

debugHook msg = print msg

main :: IO ()
main = do
    tid <- initMtGoxStream [debugHookSetup, channelJoinerHookSetup []]
    threadDelay 1000000

    forever $ threadDelay 1000000
