module Network.MtGoxAPI.DepthStore.Adapter
    ( updateDepthStoreFromMessage
    , updateDepthStoreFromFullDepth
    ) where

import Network.MtGoxAPI.Types
import Network.MtGoxAPI.DepthStore

updateDepthStoreFromMessage :: DepthStoreHandle -> StreamMessage -> IO ()
updateDepthStoreFromMessage handle (du@DepthUpdateUSD {}) = do
    let t = case duType du of
                Ask -> DepthStoreAsk
                Bid -> DepthStoreBid
    updateDepthStore handle t (duVolume du) (duPrice du)
updateDepthStoreFromMessage _ _ = return ()

updateDepthStoreFromFullDepth :: DepthStoreHandle -> FullDepth -> IO ()
updateDepthStoreFromFullDepth handle depth = do
    mapM_ (updateCmd DepthStoreAsk) (fdAsks depth)
    mapM_ (updateCmd DepthStoreBid) (fdBids depth)
    setHasFullDepth handle
  where
    updateCmd t (DepthEntry { deAmount = amount, dePrice = price }) =
        updateDepthStore handle t amount price
