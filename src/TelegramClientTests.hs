module TelegramClientTests (runTelegramTests) where

import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Data.Maybe ( fromJust, isNothing )

import qualified TelegramClient as TGC

-- Public
runTelegramTests :: Maybe String -> IO ()
runTelegramTests tokenMaybe = do
    manager <- newManager tlsManagerSettings
    if isNothing tokenMaybe
        then print "Cannot load token from config"
    else do
        let token = fromJust tokenMaybe
        TGC.getMe token manager >>= print
        TGC.getMessageUpdates token manager Nothing >>= print
-- /Public
