-- |
-- How to use:
--
-- > main :: IO ()
-- > main = do
-- >     getTickerStatus <- initTickerMonitor
-- >     getTickerStatus >>= print
--
{-# LANGUAGE OverloadedStrings #-}
module TickerMonitor
    ( initTickerMonitor
    , TickerStatus(..)
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Watchdog
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.List
import Data.String
import Data.Time.Clock
import Network
import System.IO

import qualified Data.Attoparsec as AP
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.Text as T

data TickerStatus = TickerStatus { tsTimestamp :: UTCTime
                                 , tsBuy :: Integer
                                 , tsSell :: Integer
                                 , tsLast :: Integer
                                 , tsPrecision :: Integer
                                 }
                    | TickerUnavailable
                    deriving (Show)

data StreamMessage = MiscMessage | TickerUpdate Integer Integer Integer
                     deriving (Show)

instance FromJSON StreamMessage
  where
    parseJSON (Object o) = case extractTickerData o of
            Just (buyV, sellV, lastV) -> do
                buyS <- parseJSON buyV :: Parser String
                sellS <- parseJSON sellV :: Parser String
                lastS <- parseJSON lastV :: Parser String
                return $ TickerUpdate (read buyS) (read sellS) (read lastS)
            Nothing -> return MiscMessage
    parseJSON _ = mzero

extractTickerData :: (IsString k, Ord k) => M.Map k Value -> Maybe (Value, Value, Value)
extractTickerData o = do
    buyPrice <- lookupInTicker "buy" o
    sellPrice <- lookupInTicker "sell" o
    lastPrice <- lookupInTicker "last" o
    return (buyPrice, sellPrice, lastPrice)

lookupInTicker :: (IsString k, Ord k) => T.Text -> M.Map k Value -> Maybe Value
lookupInTicker field o = do
        tickerLast <- M.lookup "ticker" o
                        >>= extractObject
                        >>= M.lookup field
                        >>= extractObject
        currency <- M.lookup "currency" tickerLast
        guard (currency == expectedCurrency)
        M.lookup "value_int" tickerLast
  where
    extractObject (Object o') = Just o'
    extractObject _ = Nothing

expectedCurrency :: Value
expectedCurrency = "USD"

expectedPrecision :: Integer
expectedPrecision = 5

parseFrame :: String -> Either String StreamMessage
parseFrame = collapseErrors . parseStreamMessage
  where
    parseStreamMessage :: String -> Either String (Result StreamMessage)
    parseStreamMessage = AP.parseOnly (fromJSON <$> json) . B8.pack

collapseErrors :: Either String (Result b) -> Either String b
collapseErrors (Left err) = Left err
collapseErrors (Right (Error err)) = Left err
collapseErrors (Right (Success payload)) = Right payload


clientHandshake :: String
clientHandshake = "GET /mtgox HTTP/1.1\r\n"
                    ++ "Upgrade: WebSocket\r\n"
                    ++ "Connection: Upgrade\r\n"
                    ++ "Host: websocket.mtgox.com\r\n"
                    ++ "Origin: null\r\n"
                    ++ "\r\n"

initialCmds :: [String]
initialCmds = [ "{\"op\":\"unsubscribe\",\"channel\":\"24e67e0d-1cad-4cc0-9e7a-f8523ef460fe\"}"
              , "{\"op\":\"unsubscribe\",\"channel\":\"dbf1dee9-4f2e-4a08-8cb7-748919a71b21\"}"
              ]

skipHeader :: Handle -> IO ()
skipHeader h = go ""
  where
    go buffer
      | "\r\n\r\n" `isSuffixOf`  buffer = return ()
      | otherwise = do
            c <- hGetChar h
            go (buffer ++ [c])

sendFrame :: Handle -> String -> IO ()
sendFrame h frame = do
    hPutChar h '\x00'
    hPutStr h frame
    hPutChar h '\xFF'

readFrame :: Handle -> MVar (Maybe TickerStatus) -> IO (Either String ())
readFrame h tickerStore = do
    _ <- hGetChar h
    frame <- readUntilFF ""
    case parseFrame frame of
        Left err -> return $ Left err
        Right msg -> case msg of
            TickerUpdate buyPrice sellPrice lastPrice -> do
                now <- getCurrentTime
                let tickerStatus = TickerStatus { tsTimestamp = now
                                                , tsBuy = buyPrice
                                                , tsSell = sellPrice
                                                , tsLast = lastPrice
                                                , tsPrecision = expectedPrecision
                                                }
                _ <- swapMVar tickerStore (Just tickerStatus)
                return $ Right ()
            _ -> return $ Right ()
  where
    readUntilFF buffer = do
        c <- hGetChar h
        if c == chr 255
            then return buffer
            else readUntilFF (buffer ++ [c])

readUntilParseError :: Handle -> MVar (Maybe TickerStatus) -> IO (Either String ())
readUntilParseError h tickerStore = do
    result <- readFrame h tickerStore
    case result of
        Right _ -> readUntilParseError h tickerStore
        Left err -> return $ Left err

websocketLoop :: HostName-> PortNumber-> MVar (Maybe TickerStatus)-> IO (Either String ())
websocketLoop uri port tickerStore = do
    status <- try go ::  IO (Either IOException (Either String ()))
    case status of
        Right result -> return result
        Left ex -> return $ Left (show ex)
  where
    go = withSocketsDo $ do
        h <- connectTo uri (PortNumber port)
        hSetBuffering h NoBuffering
        hPutStr h clientHandshake >> skipHeader h
        mapM_ (sendFrame h) initialCmds
        readUntilParseError h tickerStore

getTickerStatus :: MVar (Maybe TickerStatus) -> IO TickerStatus
getTickerStatus store =
    let watchdogConf = customWatchdogConfig 200000 {- 200 ms -}
                                            200000000 {- 20 seconds -}
                                            (\_ _ -> return ()) {- no logging -}
        task = getTickerStatus' store
    in watchdog watchdogConf task

getTickerStatus' :: MVar (Maybe TickerStatus) -> IO (WatchdogTaskStatus TickerStatus)
getTickerStatus' store = do
    tickerStatusM <- readMVar store
    case tickerStatusM of
        Nothing -> return $ FailedImmediately "No ticker data present"
        Just tickerStatus -> do
            now <- getCurrentTime
            let age = diffUTCTime now (tsTimestamp tickerStatus)
            return $ decideOnAge age tickerStatus
  where
    decideOnAge age tickerStatus
      | age < 60 = CompletedSuccessfully tickerStatus
      | age >= 60 && age <= 300 = FailedImmediately "Data stale"    -- will retry
      | otherwise = CompletedSuccessfully TickerUnavailable     -- give up

-- | Will start a thread that monitors Mt.Gox's ticker
-- and will return a function that can be used to access
-- the latest ticker status in a safe way (it will ensure
-- that fresh data is returned).
initTickerMonitor :: IO (IO TickerStatus)
initTickerMonitor = do
    store <- newMVar Nothing
    let watchdogConf = defaultWatchdogConfig
        task = websocketLoop "websocket.mtgox.com" 80 store
    _ <- forkIO $ watchdog watchdogConf (timeTask task)
    return $ getTickerStatus store

main :: IO ()
main = do
    getTickerStatus <- initTickerMonitor
    getTickerStatus >>= print
