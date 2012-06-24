module HookUtils
    ( Hook
    , HookSetup
    ) where

import StreamCommand
import StreamParsing

-- | Describes a function, that will be called when a new connection
-- to Mt.Gox is opened and supplied with a writer for this connection.
-- In turn, it returns a 'Hook' to be used while the connection is active.
-- Important: This function might get called multiple times, if the connection
-- breaks down and is re-established.
type HookSetup = StreamWriter -> IO Hook

-- | Describes a function that will be supplied with messages coming over the
-- connection from Mt.Gox
type Hook = StreamMessage -> IO ()
