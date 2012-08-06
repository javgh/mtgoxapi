module Network.MtGoxAPI.DepthStore.StressTest
    (
    ) where

import Control.Concurrent
import Control.Monad
import Data.Time.Clock
import System.Random

import Network.MtGoxAPI.DepthStore

addRandomEntry :: DepthStoreChan -> IO ()
addRandomEntry chan = do
    t <- randomIO >>= \x -> if x
                                then return DepthStoreAsk
                                else return DepthStoreBid
    amount <- randomRIO (1 * 10^8, 10 * 10^8)
    price <- randomRIO (1 * 10^5, 8 * 10^5)
    updateDepthStore chan t amount price

stressTest = do
    chan <- initDepthStore
    getCurrentTime >>= print
    forever $ do
        replicateM_ 25000 $ addRandomEntry chan
        simulateBTCSell chan 0
        getCurrentTime >>= print

main = stressTest
