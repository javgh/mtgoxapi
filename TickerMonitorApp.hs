import Control.Concurrent
import Control.Monad

import ChannelJoiner
import MtGoxStream
import TickerMonitor

main :: IO ()
main = do
    (tickerHook, getTickerStatus) <- initTickerMonitor
    tid <- initMtGoxStream [ tickerHook
                           , channelJoinerHook [mtGoxTickerChannel]
                           ]
    forever $ do
        getTickerStatus >>= print
        threadDelay 1000000
