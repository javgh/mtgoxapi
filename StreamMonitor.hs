{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Monad

import ChannelJoiner
import MtGoxStream

debugHook msg _ = print msg

main :: IO ()
main = do
    tid <- initMtGoxStream [debugHook, channelJoinerHook [mtGoxDepthChannel]]
    forever $ threadDelay 1000000
