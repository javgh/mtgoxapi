module DepthMonitor where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Maybe
import Text.Printf

import AuthCommand
import AuthCommandReplyParsing
import ChannelJoiner
import CommandHook
import DepthStore
import DepthStoreAdapter
import MtGoxStream
import StreamCommand
import StreamParsing

main :: IO ()
main = do
    (commandHookChan, commandHookSetup) <- initCommandHook
    (depthStoreChan, depthStoreHookSetup) <- initDepthStoreAdapter commandHookChan
    tid <- initMtGoxStream [ commandHookSetup
                           , channelJoinerHookSetup [mtGoxDepthChannel]
                           , depthStoreHookSetup
                           ]
    threadDelay $ 8 * 10^6
    forever $ do
        putStrLn ""
        putStrLn "-- Fees --"
        simulateCircle depthStoreChan $ 1* 10^5
        simulateCircle depthStoreChan $ 2 * 10^5
        simulateCircle depthStoreChan $ 5 * 10^5
        simulateCircle depthStoreChan $ 10 * 10^5
        simulateCircle depthStoreChan $ 15 * 10^5
        simulateCircle depthStoreChan $ 20 * 10^5
        simulateCircle depthStoreChan $ 100 * 10^5
        simulateCircle depthStoreChan $ 500 * 10^5
        simulateCircle depthStoreChan $ 1000 * 10^5
        simulateCircle depthStoreChan $ 2000 * 10^5
        simulateCircle depthStoreChan $ 5000 * 10^5
        threadDelay $ 10 * 10^6

mtgoxFee :: Double
mtgoxFee = 0.006
--mtgoxFee = 0.0025

afterMtgoxFee :: Integer -> Integer
afterMtgoxFee = round . (* (1 - mtgoxFee)) . fromIntegral

simulateCircle :: DepthStoreChan -> Integer -> IO ()
simulateCircle chan usdAmount = do
    btc <- fromMaybe 0 <$> simulateUSDSell chan usdAmount
    let btc' = afterMtgoxFee btc
    usd <- fromMaybe 0 <$> simulateBTCSell chan btc'
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
    let a' = fromIntegral a / 10 ^ 5 :: Double
    in printf "%.5f USD" a'

formatBTC :: Integer -> String
formatBTC a = 
    let a' = fromIntegral a / 10 ^ 8 :: Double
    in printf "%.8f BTC" a'

formatPercent :: Double -> String
formatPercent = printf "%.2f %%"
