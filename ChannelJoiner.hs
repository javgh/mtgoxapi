module ChannelJoiner
    ( channelJoinerHookSetup
    ) where

import Control.Monad

import qualified Data.Text as T

import HookUtils
import StreamCommand
import StreamParsing

channelJoinerHookSetup :: [T.Text] -> StreamWriter -> IO Hook
channelJoinerHookSetup channels writer = return $ channelJoinerHook channels writer

channelJoinerHook :: [T.Text] -> StreamWriter -> StreamMessage -> IO ()
channelJoinerHook channels writer (Subscribed { sChannel = sChannel }) =
    when (sChannel `notElem` channels) $
        writer (UnsubscribeCmd sChannel)
channelJoinerHook channels _ _ = return ()
