module Network.MtGoxAPI.DepthStore.Monitor
    ( monitorDepth
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Maybe
import Text.Printf

import Network.MtGoxAPI.DepthStore

monitorDepth :: DepthStoreHandle -> IO ()
monitorDepth handle = forever $ do
    putStrLn ""
    putStrLn "-- Fees --"
    simulateCircle handle $ 1* 10^(5::Integer)
    simulateCircle handle $ 2 * 10^(5::Integer)
    simulateCircle handle $ 5 * 10^(5::Integer)
    simulateCircle handle $ 10 * 10^(5::Integer)
    simulateCircle handle $ 15 * 10^(5::Integer)
    simulateCircle handle $ 20 * 10^(5::Integer)
    simulateCircle handle $ 100 * 10^(5::Integer)
    simulateCircle handle $ 500 * 10^(5::Integer)
    simulateCircle handle $ 1000 * 10^(5::Integer)
    simulateCircle handle $ 2000 * 10^(5::Integer)
    simulateCircle handle $ 5000 * 10^(5::Integer)
    threadDelay $ 10 * 10^(6::Integer)

mtgoxFee :: Double
mtgoxFee = 0.006

afterMtgoxFee :: Integer -> Integer
afterMtgoxFee = round . (* (1 - mtgoxFee)) . fromIntegral

simulateCircle :: DepthStoreHandle -> Integer -> IO ()
simulateCircle handle usdAmount = do
    btc <- fromMaybe 0 <$> simulateUSDSell handle usdAmount
    let btc' = afterMtgoxFee btc
    usd <- fromMaybe 0 <$> simulateBTCSell handle btc'
    let usd' = afterMtgoxFee usd
        cost = usdAmount - usd'
        inPercent = if usdAmount > 0
                        then (fromIntegral cost * 100) / fromIntegral usdAmount
                        else 0
    putStrLn $ formatUSD usdAmount ++ ": " ++ formatPercent (inPercent / 2)
                ++ " per transaction fee"
    putStrLn $ "\tSell " ++ formatUSD usdAmount ++ " -->  "
                ++ formatBTC btc ++ " --> after fees --> "
                ++ formatBTC btc' ++ " --> sell again\n\t --> "
                ++ formatUSD usd ++ " --> after fees --> "
                ++ formatUSD usd' ++ " --> fees in percent: "
                ++ formatPercent inPercent

formatUSD :: Integer -> String
formatUSD a =
    let a' = fromIntegral a / 10 ^ (5 :: Integer) :: Double
    in printf "%.5f EUR" a'

formatBTC :: Integer -> String
formatBTC a =
    let a' = fromIntegral a / 10 ^ (8 :: Integer) :: Double
    in printf "%.8f BTC" a'

formatPercent :: Double -> String
formatPercent = printf "%.2f %%"
