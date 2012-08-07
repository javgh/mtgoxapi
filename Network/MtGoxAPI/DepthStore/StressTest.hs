module Network.MtGoxAPI.DepthStore.StressTest
    ( stressTest
    ) where

import Control.Monad
import Data.Time.Clock
import System.Random

import Network.MtGoxAPI.DepthStore

addRandomEntry :: DepthStoreHandle -> IO ()
addRandomEntry handle = do
    t <- randomIO >>= \x -> if x
                                then return DepthStoreAsk
                                else return DepthStoreBid
    amount <- randomRIO (1 * 10^(8::Integer), 10 * 10^(8::Integer))
    price <- randomRIO (1 * 10^(5::Integer), 8 * 10^(5::Integer))
    updateDepthStore handle t amount price

stressTest :: IO ()
stressTest = do
    handle <- initDepthStore
    getCurrentTime >>= print
    forever $ do
        replicateM_ 25000 $ addRandomEntry handle
        _ <- simulateBTCSell handle 0
        getCurrentTime >>= print
