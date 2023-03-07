module TelegramClientTests where

import TelegramClient ( getMe )
import Network.HTTP.Client ( newManager )
import ConfigReader ( readConfig, Config(token) )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Data.Maybe ( fromJust, isNothing )

configPath :: String
configPath = "config.json"

getToken :: IO (Maybe String)
getToken = do
    config <- readConfig configPath
    return $ token <$> config

runTelegramTests :: IO ()
runTelegramTests = do
    manager <- newManager tlsManagerSettings
    tokenMaybe <- getToken
    if isNothing tokenMaybe
        then print "Cannot load token from config"
    else do
        let token = fromJust tokenMaybe
        getMe token manager >>= print
