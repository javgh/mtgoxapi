module HookUtils
    ( Hook
    , HookSetup
    ) where

import StreamCommand
import StreamParsing

type Hook = StreamMessage -> IO ()
type HookSetup = StreamWriter -> IO Hook
