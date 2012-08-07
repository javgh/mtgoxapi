module Network.MtGoxAPI.WalletNotifier
    ( initWalletNotifier
    , updateWalletNotifier
    , waitForBTCDeposit
    , WalletNotifierHandle
    ) where

import Control.Applicative
import Control.Concurrent

import Network.MtGoxAPI.Types

newtype WalletNotifierHandle = WalletNotifierHandle { unWNH :: MVar () }

initWalletNotifier :: IO WalletNotifierHandle
initWalletNotifier = WalletNotifierHandle <$> newEmptyMVar

updateWalletNotifier :: WalletNotifierHandle -> StreamMessage -> IO ()
updateWalletNotifier handle (wo@WalletOperation {}) =
    case woType wo of
        BTCDeposit -> do
            tryPutMVar (unWNH handle) ()   -- write to MVar, if not already full
            return ()
        _ -> return ()
updateWalletNotifier _ _ = return ()

-- | Will block until a BTC deposit happens. Note: This might sometimes be a
-- little unreliable, when the streaming connection is under load from a lot of
-- depth channel updates.
waitForBTCDeposit :: WalletNotifierHandle -> IO ()
waitForBTCDeposit handle = takeMVar (unWNH handle) >> return ()
