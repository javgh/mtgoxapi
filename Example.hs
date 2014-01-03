import Network.MtGoxAPI

import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = do
    putStrLn "Please provide your API key: "
    authKey <- getLine
    putStrLn "Please provide your API secret: "
    authSecret <- getLine
    let credentials = initMtGoxCredentials (B8.pack authKey)
                                           (B8.pack authSecret)
        streamSettings = MtGoxStreamSettings DisableWalletNotifications 
                                             SkipFullDepth
    apiHandles <- initMtGoxAPI Nothing credentials streamSettings
    getPrivateInfoR apiHandles >>= print
