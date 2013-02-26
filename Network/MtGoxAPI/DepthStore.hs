{-# LANGUAGE BangPatterns, DeriveDataTypeable, CPP #-}

module Network.MtGoxAPI.DepthStore
    ( initDepthStore
    , updateDepthStore
    , setHasFullDepth
    , simulateBTCSell
    , simulateBTCBuy
    , simulateUSDSell
    , simulateUSDBuy
    , DepthStoreHandle
    , DepthStoreType(..)
    , DepthStoreAnswer(..)
#if !PRODUCTION
    , simulateBTC
    , simulateUSD
    , DepthStoreEntry (..)
#endif
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Watchdog
import Data.IxSet((@<), (@>), (@>=), (@<=))
import Data.Time.Clock
import Data.Typeable

import qualified Data.IxSet as I

staleInterval :: NominalDiffTime
staleInterval = -1 * 60 * 60 * 24   -- remove entries older than one day

intervalToRemove :: NominalDiffTime
intervalToRemove = -1 * 60          -- remove in blocks of these, so we do
                                    -- not have to do it as often

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

data DepthStoreAnswer = DepthStoreAnswer Integer
                      | NotEnoughDepth
                      | DepthStoreUnavailable
                      deriving (Show)

data DepthStoreType = DepthStoreAsk | DepthStoreBid
                      deriving (Show)

data DepthStoreData = DepthStoreData { dsdAskStore :: I.IxSet DepthStoreEntry
                                     , dsdBidStore :: I.IxSet DepthStoreEntry
                                     , dsdHasFullDepth :: Bool
                                     , dsdLastUpdate :: Maybe UTCTime
                                     }
                      deriving (Show)

data DepthStoreHandle = DepthStoreHandle
                            { _unDSH :: MVar DepthStoreData }

initDepthStore :: IO DepthStoreHandle
initDepthStore = do
    let dsd = DepthStoreData { dsdAskStore = I.empty
                             , dsdBidStore = I.empty
                             , dsdHasFullDepth = False
                             , dsdLastUpdate = Nothing
                             }
    DepthStoreHandle <$> newMVar dsd

setHasFullDepth :: DepthStoreHandle -> IO ()
setHasFullDepth (DepthStoreHandle dsdMVar) = do
    dsd <- takeMVar dsdMVar
    putMVar dsdMVar dsd { dsdHasFullDepth = True }
    return ()

updateDepthStore :: DepthStoreHandle -> DepthStoreType -> Integer -> Integer -> IO ()
updateDepthStore (DepthStoreHandle dsdMVar) t amount price = do
    dsd <- readMVar dsdMVar     -- Read and put it back, so that we
                                -- do not block other readers while we
                                -- are doing the update. It is no problem if
                                -- they receive slightly outdated data and it is
                                -- safe, since we are the only producer.
    timestamp <- getCurrentTime
    askStore' <- removeStaleEntries $ dsdAskStore dsd
    bidStore' <- removeStaleEntries $ dsdBidStore dsd
    let (askStore'', bidStore'') =
            case t of
                DepthStoreAsk ->
                    (askStore', removeConflictingBidEntries bidStore' price)
                DepthStoreBid ->
                    (removeConflictingAskEntries askStore' price, bidStore')
    let (askStore''', bidStore''') =
            case t of
                DepthStoreAsk ->
                    (updateStore askStore'' amount price timestamp, bidStore'')
                DepthStoreBid ->
                    (askStore'', updateStore bidStore'' amount price timestamp)
    _ <- swapMVar dsdMVar dsd { dsdAskStore = askStore'''
                              , dsdBidStore = bidStore'''
                              , dsdLastUpdate = Just timestamp
                              }
    return ()

isDataFresh :: DepthStoreHandle -> IO (Either String ())
isDataFresh (DepthStoreHandle dsdMVar) = do
    dsd <- readMVar dsdMVar
    now <- getCurrentTime
    return $ decide (dsdHasFullDepth dsd) (dsdLastUpdate dsd) now
  where
    decide False _ _ = Left "Full depth not yet available."
    decide True Nothing _ = Left "Depth store is still empty."
    decide True (Just timestamp) now =
        let age = diffUTCTime now timestamp
        in if age > 180
            then Left "Depth store data is stale"
            else Right ()

repeatSimulation :: DepthStoreHandle -> IO (Maybe Integer) -> IO DepthStoreAnswer
repeatSimulation handle simulationAction = do
    let task = do
            isFresh <- isDataFresh handle
            case isFresh of
                Left msg -> return $ Left msg
                Right _ -> Right <$> simulationAction
    result <- watchdog $ do
                setLoggingAction silentLogger
                watchImpatiently task
    return $ case result of
        Left _ -> DepthStoreUnavailable
        Right v -> case v of
            Just v' -> DepthStoreAnswer v'
            Nothing -> NotEnoughDepth

-- | Simulate how much USD can be earned by selling the specified amount of BTC.
-- The function will return 'NotEnoughDepth' in case there is not enough depth
-- to cover the full amount. If not recent data is available, it will return
-- 'DepthStoreUnavailable'. In the latter case it will have retried a few times
-- before giving up.
simulateBTCSell :: DepthStoreHandle -> Integer -> IO DepthStoreAnswer
simulateBTCSell handle@(DepthStoreHandle dsdMVar) amount =
    repeatSimulation handle simulation
  where
    simulation = do
        dsd <- readMVar dsdMVar
        let bids = I.toDescList (I.Proxy :: I.Proxy Integer) $ dsdBidStore dsd
        return $ simulateBTC amount bids

-- | Simulate how much USD will be needed to buy the specified amount of BTC.
-- See 'simulateBTCSell' for more details.
simulateBTCBuy :: DepthStoreHandle -> Integer -> IO DepthStoreAnswer
simulateBTCBuy handle@(DepthStoreHandle dsdMVar) amount =
    repeatSimulation handle simulation
  where
    simulation = do
        dsd <- readMVar dsdMVar
        let asks = I.toAscList (I.Proxy :: I.Proxy Integer) $ dsdAskStore dsd
        return $ simulateBTC amount asks

-- | Simulate how much BTC can be earned by selling the specified amount of USD.
-- See 'simulateBTCSell' for more details.
simulateUSDSell :: DepthStoreHandle -> Integer -> IO DepthStoreAnswer
simulateUSDSell handle@(DepthStoreHandle dsdMVar) usdAmount =
    repeatSimulation handle simulation
  where
    simulation = do
        dsd <- readMVar dsdMVar
        let asks = I.toAscList (I.Proxy :: I.Proxy Integer) $ dsdAskStore dsd
        return $ simulateUSD usdAmount asks

-- | Simulate how much BTC will be needed to buy the specified amount of USD.
-- See 'simulateBTCSell' for more details.
simulateUSDBuy :: DepthStoreHandle -> Integer -> IO DepthStoreAnswer
simulateUSDBuy handle@(DepthStoreHandle dsdMVar) usdAmount =
    repeatSimulation handle simulation
  where
    simulation = do
        dsd <- readMVar dsdMVar
        let bids = I.toDescList (I.Proxy :: I.Proxy Integer) $ dsdBidStore dsd
        return $ simulateUSD usdAmount bids

updateStore :: I.IxSet DepthStoreEntry-> Integer -> Integer -> UTCTime -> I.IxSet DepthStoreEntry
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
        -- We will do a fast check first, to avoid constantly moving stuff
        -- around in memory. In addition, we go a little further into the past,
        -- so that when the check fires, we have a bunch of stuff to remove at
        -- once.
        fastCutoffCheck = addUTCTime intervalToRemove cutoff
    if I.null (store @< fastCutoffCheck)
        then return store
        else return $ store @>= cutoff


-- | Keep internal consistency, by removing bids that a higher than
-- the new ask, which means they must have already been filled.
removeConflictingBidEntries :: (Ord a, Typeable k, Typeable a, I.Indexable a) =>I.IxSet a -> k -> I.IxSet a
removeConflictingBidEntries !bidStore askPrice =
    if I.null (bidStore @>= askPrice)
        then bidStore
        else bidStore @< askPrice

-- | Keep internal consistency, by removing asks that a lower than
-- the new bid, which means they must have already been filled.
removeConflictingAskEntries :: (Ord a, Typeable k, Typeable a, I.Indexable a) =>I.IxSet a -> k -> I.IxSet a
removeConflictingAskEntries !askStore bidPrice =
    if I.null (askStore @<= bidPrice)
        then askStore
        else askStore @> bidPrice

simulateBTC :: Integer -> [DepthStoreEntry] -> Maybe Integer
simulateBTC 0 _ = Just 0
simulateBTC _ [] = Nothing
simulateBTC remainingAmount ((dse@DepthStoreEntry {}):entries) =
    let amount = dseAmount dse
        price = dsePrice dse
    in if remainingAmount <= amount
            then Just (adjustZeros (remainingAmount * price))
            else let x = adjustZeros (amount * price)
                     y = simulateBTC (remainingAmount - amount) entries
                 in (+) x <$> y
  where
    adjustZeros = round . (/ (10 ^ (8 :: Integer) :: Double)) . fromIntegral

simulateUSD :: Integer -> [DepthStoreEntry] -> Maybe Integer
simulateUSD 0 _ = Just 0
simulateUSD _ [] = Nothing
simulateUSD remainingUsdAmount ((dse@DepthStoreEntry {}):entries) =
    let amount = dseAmount dse
        price = dsePrice dse
        totalCost = adjustZeros(amount * price)
    in if remainingUsdAmount <= totalCost
            then Just (adjustedDevide remainingUsdAmount price)
            else let x = amount
                     y = simulateUSD (remainingUsdAmount - totalCost) entries
                 in (+) x <$> y
  where
    adjustZeros = round . (/ (10 ^ (8 :: Integer) :: Double)) . fromIntegral
    adjustedDevide a b = round . (/ (fromIntegral b :: Double))
                            . fromIntegral . (* 10 ^ (8 :: Integer)) $ a
