import Control.Concurrent
import Control.Monad

import MtGoxStream
import TickerMonitor

main :: IO ()
main = do
    (tickerHook, getTickerStatus) <- initTickerMonitor
    tid <- initMtGoxStream [tickerHook]
    forever $ do
        getTickerStatus >>= print
        threadDelay 1000000
