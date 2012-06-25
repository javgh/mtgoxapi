module DepthStoreAdapter
    ( initDepthStoreAdapter
    ) where

import Control.Applicative
import Control.Concurrent

import AuthCommandReplyParsing
import CommandHook
import DepthStore
import HookUtils
import StreamParsing
import StreamCommand

-- TODO: find better solution for delayed init

initDepthStoreAdapter :: CommandHookChan -> IO (DepthStoreChan, HookSetup)
initDepthStoreAdapter commandHookChan = do
    depthStoreChan <- initDepthStore
    _ <- forkIO $ delayedInit commandHookChan depthStoreChan
    return (depthStoreChan, depthStoreHookSetup depthStoreChan)

depthStoreHookSetup :: DepthStoreChan -> StreamWriter -> IO Hook
depthStoreHookSetup depthStoreChan _ = return $ depthStoreHook depthStoreChan

depthStoreHook :: DepthStoreChan -> StreamMessage -> IO ()
depthStoreHook depthStoreChan (du@DepthUpdateUSD {}) = do
    let t = case duType du of
                Ask -> DepthStoreAsk
                Bid -> DepthStoreBid
    updateDepthStore depthStoreChan t (duVolume du) (duPrice du)
depthStoreHook _ _ = return ()

debugInit :: CommandHookChan -> DepthStoreChan -> IO ()
debugInit commandHookChan depthStoreChan = do
    putStrLn "starting debug init of depth store"
    depth <- read <$> readFile "/home/jan/year/12_26/Mon_0625/fulldepth"
    print "number of asks:"
    print $ length (fdrAsks depth)
    print "number of bids:"
    print $ length (fdrBids depth)
    mapM_ (updateCmd DepthStoreAsk) (fdrAsks depth)
    mapM_ (updateCmd DepthStoreBid) (fdrBids depth)
    putStrLn "depth store initialized"
    testCall "buy 1 BTC" $ simulateBTCBuy depthStoreChan (1 * 10^8)
    testCall "buy 10 BTC" $ simulateBTCBuy depthStoreChan (10 * 10^8)
    testCall "buy 100 BTC" $ simulateBTCBuy depthStoreChan (100 * 10^8)
    testCall "buy 1000 BTC" $ simulateBTCBuy depthStoreChan (1000 * 10^8)
    testCall "buy 10000 BTC" $ simulateBTCBuy depthStoreChan (10000 * 10^8)
    testCall "buy 100000 BTC" $ simulateBTCBuy depthStoreChan (100000 * 10^8)
    testCall "buy 1000000 BTC" $ simulateBTCBuy depthStoreChan (1000000 * 10^8)
    testCall "buy 10000000 BTC" $ simulateBTCBuy depthStoreChan (10000000 * 10^8)
    testCall "buy 100000000 BTC" $ simulateBTCBuy depthStoreChan (100000000 * 10^8)
  where
    updateCmd t (DepthEntry { deAmount = amount, dePrice = price }) =
        updateDepthStore depthStoreChan t amount price
    testCall msg call = do
        p <- call
        putStrLn $ msg ++ ": " ++ show p

delayedInit :: CommandHookChan -> DepthStoreChan -> IO ()
delayedInit commandHookChan depthStoreChan = do
    threadDelay 5000000
    putStrLn "starting delayed init of depth store"
    depthM <- sendFullDepthCmd commandHookChan
    case depthM of
        Just (depth) -> do
            mapM_ (updateCmd DepthStoreAsk) (fdrAsks depth)
            mapM_ (updateCmd DepthStoreBid) (fdrBids depth)
        Nothing -> return ()
    putStrLn "depth store initialized"
    price1 <- simulateBTCSell depthStoreChan (10 * 10^8)
    price2 <- simulateBTCBuy depthStoreChan (10 * 10^8)
    putStrLn "simulateBTCSell of 10 BTC:"
    print price1
    putStrLn "simulateBTCBuy of 10 BTC:"
    print price2
  where
    updateCmd t (DepthEntry { deAmount = amount, dePrice = price }) =
        updateDepthStore depthStoreChan t amount price
