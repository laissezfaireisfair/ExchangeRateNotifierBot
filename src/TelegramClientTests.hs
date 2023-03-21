module TelegramClientTests where

import TelegramClient ( getMe, getMessageUpdates )
import Network.HTTP.Client ( newManager )
import ConfigReader ( readConfig, Config(token) )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Data.Maybe ( fromJust, isNothing )

runTelegramTests :: Maybe String -> IO ()
runTelegramTests tokenMaybe = do
    manager <- newManager tlsManagerSettings
    if isNothing tokenMaybe
        then print "Cannot load token from config"
    else do
        let token = fromJust tokenMaybe
        getMe token manager >>= print
        getMessageUpdates token manager Nothing >>= print
