module CommandHook
    (
    ) where

import Control.Concurrent

import HookUtils
import StreamCommand

initCommandHook :: IO HookSetup
initCommandHook = do
    chan <- newChan :: IO (Chan ())
    writerStore <- newMVar (Nothing :: Maybe StreamWriter)
    --_ <- forkIO $ watchChannel chan writerStore
    return $ commandHookSetup chan writerStore

commandHookSetup chan writerStore writer = do
    _ <- swapMVar writerStore (Just writer)
    return $ commandHook chan

commandHook chan = print
