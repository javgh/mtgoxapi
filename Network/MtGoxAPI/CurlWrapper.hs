module Network.MtGoxAPI.CurlWrapper
    ( initCurlWrapper
    , performCurlRequest
    , CurlHandle
    ) where

import Control.Applicative
import Control.Concurrent
import Data.IORef
import Network.Curl

data CurlData = CurlData { cdUrl :: URLString
                         , cdOpts :: [CurlOption]
                         , cdAnswerChan :: Chan (CurlCode, String)
                         }

newtype CurlHandle = CurlHandle { unCH :: Chan CurlData }

-- | Will start a thread which will execute cURL requests that are passed to it
-- using 'performCurlRequest'. Internally only a single cURL handle is opened,
-- which means that keep-alive connections are automatically reused.
initCurlWrapper :: IO CurlHandle
initCurlWrapper = do
    chan <- newChan :: IO (Chan CurlData)
    _ <- forkIO $ curlThread chan
    return $ CurlHandle chan

curlThread :: Chan CurlData -> IO ()
curlThread requestChan = withCurlDo $ do
    handle <- initialize
    _ <- setopt handle (CurlVerbose False)
    _ <- setopt handle (CurlUserAgent "libcurl")
    _ <- setopt handle (CurlFailOnError True)
    _ <- setopt handle (CurlSSLVerifyPeer False)
    _ <- setopt handle (CurlSSLVerifyHost 0)
    go handle
  where
    go h = do
        CurlData url opts answerChan <- readChan requestChan
        ref <- newIORef []
        _ <- setopt h (CurlURL url)
        _ <- setopt h (CurlWriteFunction (gatherOutput ref))
        mapM_ (setopt h) opts
        rc <- perform h
        body <- concat . reverse <$> readIORef ref
        writeChan answerChan (rc, body)
        go h

performCurlRequest :: CurlHandle -> URLString -> [CurlOption] -> IO (CurlCode, String)
performCurlRequest curlHandle url opts = do
    answerChan <- newChan
    let cd = CurlData { cdUrl = url
                      , cdOpts = opts
                      , cdAnswerChan = answerChan
                      }
    writeChan (unCH curlHandle) cd
    readChan answerChan
