module ChannelJoiner
    ( channelJoinerHook
    ) where

import Control.Monad

import qualified Data.Text as T

import StreamCommand
import StreamParsing

channelJoinerHook :: [T.Text] -> StreamMessage -> StreamWriter -> IO ()
channelJoinerHook channels (Subscribed { sChannel = sChannel }) writer =
    when (sChannel `notElem` channels) $
        writer (UnsubscribeCmd sChannel)
channelJoinerHook channels _ _ = return ()
