{-# LANGUAGE BangPatterns, DeriveDataTypeable, CPP #-}
module DepthStore
    ( DepthStoreChan
    , DepthStoreType(..)
    , initDepthStore
    , simulateBTCSell
    , simulateBTCBuy
    , updateDepthStore
#if !PRODUCTION
    , simulate
    , DepthStoreEntry (..)
#endif
    ) where

import Control.Applicative
import Control.Concurrent
import Data.IxSet((@<), (@>=))
import Data.Time.Clock
import Data.Typeable

import qualified Data.IxSet as I

staleInterval :: NominalDiffTime
staleInterval = -1 * 60 * 60 * 24   -- remove entries older than one day

data DepthStoreEntry = DepthStoreEntry { dseAmount :: Integer
                                       , dsePrice :: Integer
                                       , dseTimestamp :: UTCTime
                                       }
                                       deriving (Eq, Ord, Show, Typeable)

instance I.Indexable DepthStoreEntry
  where
    empty = I.ixSet [ I.ixFun $ \e -> [ dsePrice e ]
                    , I.ixFun $ \e -> [ dseTimestamp e ]
                    ]

data DepthStoreType = DepthStoreAsk | DepthStoreBid
                      deriving (Show)

data DepthStoreMsg = UpdateDepthStore { dsmType :: DepthStoreType
                                      , dsmAmount :: Integer
                                      , dsmPrice :: Integer
                                      }
                     | SimulateBTCSell { dsmAmount :: Integer
                                       , dsmAnswerChan :: Chan (Maybe Integer)
                                       }
                     | SimulateBTCBuy { dsmAmount :: Integer
                                      , dsmAnswerChan :: Chan (Maybe Integer)
                                      }

newtype DepthStoreChan = DepthStoreChan { dscChan :: Chan DepthStoreMsg }

initDepthStore :: IO DepthStoreChan
initDepthStore = do
    cmdChan <- newChan :: IO (Chan DepthStoreMsg)
    _ <- forkIO $ depthStoreLoop cmdChan
    return $ DepthStoreChan cmdChan

depthStoreLoop :: Chan DepthStoreMsg -> IO b
depthStoreLoop cmdChan = go I.empty I.empty
  where
    go !askStore !bidStore = do
        msg <- readChan cmdChan
        askStore' <- removeStaleEntries askStore
        bidStore' <- removeStaleEntries bidStore
        --let askStore' = askStore
        --    bidStore' = bidStore
        (askStore'', bidStore'') <- processMsg msg askStore' bidStore'
        go askStore'' bidStore''

processMsg :: DepthStoreMsg-> I.IxSet DepthStoreEntry-> I.IxSet DepthStoreEntry-> IO (I.IxSet DepthStoreEntry, I.IxSet DepthStoreEntry)
processMsg UpdateDepthStore { dsmType = t
                            , dsmAmount = amount
                            , dsmPrice = price } askStore bidStore = do
    timestamp <- getCurrentTime
    return $ case t of
        DepthStoreAsk -> (updateStore askStore amount price timestamp, bidStore)
        DepthStoreBid -> (askStore, updateStore bidStore amount price timestamp)
processMsg SimulateBTCSell { dsmAmount = amount
                           , dsmAnswerChan = answerChan } askStore bidStore = do
    let bids = I.toDescList (I.Proxy :: I.Proxy Integer) bidStore
        answer = simulate amount bids
    writeChan answerChan answer
    return (askStore, bidStore)
processMsg SimulateBTCBuy { dsmAmount = amount
                          , dsmAnswerChan = answerChan } askStore bidStore = do
    let asks = I.toAscList (I.Proxy :: I.Proxy Integer) askStore
        answer = simulate amount asks
    writeChan answerChan answer
    return (askStore, bidStore)

simulate :: Integer -> [DepthStoreEntry] -> Maybe Integer
simulate 0 _ = Just 0
simulate _ [] = Nothing
simulate remainingAmount ((dse@DepthStoreEntry {}):entries) =
    let amount = dseAmount dse
        price = dsePrice dse
    in if remainingAmount <= amount
            then Just (adjustZeros (remainingAmount * price))
            else let x = adjustZeros (amount * price)
                     y = simulate (remainingAmount - amount) entries
                 in (+) x <$> y
  where
    adjustZeros = round . (/ 10 ^ 8) . fromIntegral

updateStore !store amount price timestamp =
    let entry = DepthStoreEntry { dseAmount = amount
                                , dsePrice = price
                                , dseTimestamp = timestamp
                                }
    in I.updateIx price entry store

removeStaleEntries :: (I.Indexable a, Typeable a, Ord a) => I.IxSet a -> IO (I.IxSet a)
removeStaleEntries !store = do
    now <- getCurrentTime
    let cutoff = addUTCTime staleInterval now
    -- do a fast check first
    if null (I.toList (store @< cutoff))
        then return store
        else return $ store @>= cutoff

simulateBTCSell :: DepthStoreChan -> Integer -> IO (Maybe Integer)
simulateBTCSell (DepthStoreChan cmdChan) amount = do
    answerChan <- newChan :: IO (Chan (Maybe Integer))
    writeChan cmdChan $ SimulateBTCSell amount answerChan
    readChan answerChan

simulateBTCBuy :: DepthStoreChan -> Integer -> IO (Maybe Integer)
simulateBTCBuy (DepthStoreChan cmdChan) amount = do
    answerChan <- newChan :: IO (Chan (Maybe Integer))
    writeChan cmdChan $ SimulateBTCBuy amount answerChan
    readChan answerChan

updateDepthStore :: DepthStoreChan -> DepthStoreType -> Integer -> Integer -> IO ()
updateDepthStore (DepthStoreChan cmdChan) t amount price = do
    let cmd = UpdateDepthStore { dsmType = t
                               , dsmAmount = amount
                               , dsmPrice = price }
    writeChan cmdChan cmd
