module TelegramClientTests (runTelegramTests) where

import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Data.Maybe ( fromJust, isNothing )

import qualified TelegramClient as TGC

-- Public
runTelegramTests :: Maybe String -> IO ()
runTelegramTests tokenMaybe = do
    putStrLn "Testing telegram..."
    manager <- newManager tlsManagerSettings
    if isNothing tokenMaybe
        then putStrLn "Cannot load token from config"
    else do
        let token = fromJust tokenMaybe
        TGC.getMe token manager >>= print
        updatesMb <- TGC.getMessageUpdates token manager Nothing
        case updatesMb of
            Just _ -> putStrLn "Get updates - OK"
            Nothing -> putStrLn "Get updates - FAILED"
-- /Public
