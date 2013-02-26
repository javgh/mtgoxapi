module Network.MtGoxAPI.DepthStore.Tests
    ( depthStoreTests
    ) where

import Control.Monad
import Data.Maybe
import Data.Time.Clock
import Data.Typeable
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Data.IxSet as I

import Network.MtGoxAPI.DepthStore

-- use newtype wrapper to avoid 'orphan instance' warning
newtype ArbDepthStoreEntry = ArbDepthStoreEntry { unADSE :: DepthStoreEntry }
                             deriving (Show)

data MockupDepthStoreUpdate = MockupDepthStoreUpdate
                                { _mdsuType :: DepthStoreType
                                , _mdsuAmount :: Integer
                                , _msduPrice :: Integer
                                }
                              deriving (Show)

instance Arbitrary ArbDepthStoreEntry where
    arbitrary = do
        let timestamp = read "2012-06-25 00:00:00 UTC" :: UTCTime
        amount <- choose (1 * 10^(8::Integer), 10 * 10^(8::Integer))
        price <- choose (1 * 10^(5::Integer), 8 * 10^(5::Integer))
        return $ ArbDepthStoreEntry (DepthStoreEntry amount price timestamp)

instance Arbitrary MockupDepthStoreUpdate where
    arbitrary = do
        t <- elements [DepthStoreAsk, DepthStoreBid]
        amount <- choose (1 * 10^(8::Integer), 10 * 10^(8::Integer))
        price <- choose (1 * 10^(5::Integer), 8 * 10^(5::Integer))
        return $ MockupDepthStoreUpdate t amount price

returnDescending :: (I.Indexable a, Typeable a, Ord a) => [a] -> [a]
returnDescending entries =
    I.toDescList (I.Proxy :: I.Proxy Integer) $ I.fromList entries

returnAscending :: (I.Indexable a, Typeable a, Ord a) => [a] -> [a]
returnAscending entries =
    I.toAscList (I.Proxy :: I.Proxy Integer) $ I.fromList entries

propZeroAmountAlwaysOk :: [ArbDepthStoreEntry] -> Bool
propZeroAmountAlwaysOk arbEntries =
    let entries = map unADSE arbEntries
    in case simulateBTC 0 (returnDescending entries) of
        Just _ -> True
        Nothing -> False

propLowerAmountAlwaysOk :: [ArbDepthStoreEntry] -> Integer -> Integer -> Property
propLowerAmountAlwaysOk arbEntries a1 a2 =
    let entries = map unADSE arbEntries
        entries' = returnDescending entries
        sim1 = simulateBTC a1 entries'
        sim2 = simulateBTC a2 entries'
    in a1 >= 0 && a2 >= 0 && a1 >= a2 && isJust sim1 ==>
        isJust sim2 && checkTotalPriceIsHigherOrEqual sim1 sim2

propSellingAndBuyingMatches :: [ArbDepthStoreEntry] -> Integer -> Property
propSellingAndBuyingMatches arbEntries amount =
    let entries = map unADSE arbEntries
        amount' = amount * 10^(5::Integer)
        entries' = returnAscending entries
        sim1 = simulateUSD amount' entries'
        sim2 = simulateBTC (fromMaybe 0 sim1) entries'
    in amount' >= 0 && isJust sim1 ==>
        isJust sim2 && Just amount' == sim2

checkTotalPriceIsHigherOrEqual :: Ord a => Maybe a -> Maybe a -> Bool
checkTotalPriceIsHigherOrEqual (Just a) (Just b) = a >= b
checkTotalPriceIsHigherOrEqual _ _ = False

btcTradeCycleConsistency :: NonEmptyList MockupDepthStoreUpdate-> NonNegative Integer -> Property
btcTradeCycleConsistency (NonEmpty updates) (NonNegative amount) =
    tradeCycleConsistency simulateBTCSell simulateUSDSell (<=) updates amount

btcTradeCycleConsistency' :: NonEmptyList MockupDepthStoreUpdate-> NonNegative Integer -> Property
btcTradeCycleConsistency' (NonEmpty updates) (NonNegative amount) =
    tradeCycleConsistency simulateBTCBuy simulateUSDBuy (>=) updates amount

usdTradeCycleConsistency :: NonEmptyList MockupDepthStoreUpdate-> NonNegative Integer -> Property
usdTradeCycleConsistency (NonEmpty updates) (NonNegative amount) =
    tradeCycleConsistency simulateUSDSell simulateBTCSell (<=) updates amount

usdTradeCycleConsistency' :: NonEmptyList MockupDepthStoreUpdate-> NonNegative Integer -> Property
usdTradeCycleConsistency' (NonEmpty updates) (NonNegative amount) =
    tradeCycleConsistency simulateUSDBuy simulateBTCBuy (>=) updates amount

tradeCycleConsistency :: Num a =>(DepthStoreHandle -> a -> IO DepthStoreAnswer)-> (DepthStoreHandle -> Integer -> IO DepthStoreAnswer)-> (Integer -> a -> Bool)-> [MockupDepthStoreUpdate]-> a-> Property
tradeCycleConsistency firstSell secondSell cmpr updates centAmount = monadicIO $ do
    check <- run $ do
        let btcAmount = centAmount * 10 ^ (6 :: Integer)
        handle <- initDepthStore
        setHasFullDepth handle
        forM_ updates $
            \(MockupDepthStoreUpdate t amount price) ->
                updateDepthStore handle t amount price
        usdAmountM <- firstSell handle btcAmount
        case usdAmountM of
            DepthStoreUnavailable -> return False
            NotEnoughDepth -> return True
            DepthStoreAnswer usdAmount -> do
                btcAmount'M <- secondSell handle usdAmount
                case btcAmount'M of
                    DepthStoreUnavailable -> return False
                    NotEnoughDepth -> return True
                    DepthStoreAnswer btcAmount' ->
                        return (btcAmount' `cmpr` btcAmount)
    assert check

depthStoreTests :: [Test]
depthStoreTests = [ testProperty "zero amount always ok" propZeroAmountAlwaysOk
                  , testProperty "lower amount always ok" propLowerAmountAlwaysOk
                  , testProperty "selling and buying matches" propSellingAndBuyingMatches
                  , testProperty "selling BTC and buying it back yields no profit" btcTradeCycleConsistency
                  , testProperty "selling USD and buying it back yields no profit" usdTradeCycleConsistency
                  , testProperty "buying BTC in a roundabout way is not cheaper" btcTradeCycleConsistency'
                  , testProperty "buying USD in a roundabout way is not cheaper" usdTradeCycleConsistency'
                  ]
