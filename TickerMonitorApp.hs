import Control.Concurrent
import Control.Monad

import ChannelJoiner
import MtGoxStream
import TickerMonitor

main :: IO ()
main = do
    (tickerHookSetup, getTickerStatus) <- initTickerMonitor
    tid <- initMtGoxStream [ tickerHookSetup
                           , channelJoinerHookSetup [mtGoxTickerChannel]
                           ]
    forever $ do
        getTickerStatus >>= print
        threadDelay 1000000
