module CurlWrapper
    ( initCurlWrapper
    , performCurlRequest
    , CurlChan
    ) where

import Control.Applicative
import Control.Concurrent
import Data.IORef
import Network.Curl

data CurlData = CurlData { cdUrl :: URLString
                         , cdOpts :: [CurlOption]
                         , cdAnswerChan :: Chan (CurlCode, String)
                         }

newtype CurlChan = CurlChan { unCC :: Chan CurlData }

initCurlWrapper :: IO CurlChan
initCurlWrapper = do
    chan <- newChan :: IO (Chan CurlData)
    _ <- forkIO $ curlThread chan
    return $ CurlChan chan

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
        --writeFile "/tmp/debug.json" body
        --print body
        writeChan answerChan (rc, body)
        go h

performCurlRequest :: CurlChan -> URLString -> [CurlOption] -> IO (CurlCode, String)
performCurlRequest curlChan url opts = do
    answerChan <- newChan
    let cd = CurlData { cdUrl = url
                      , cdOpts = opts
                      , cdAnswerChan = answerChan
                      }
    writeChan (unCC curlChan) cd
    readChan answerChan
