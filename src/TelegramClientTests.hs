import TelegramClient
import Network.HTTP.Client
import ConfigReader ( readConfig, Config(token) )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Data.Maybe ( fromJust, isNothing )

configPath = "../config.json"

getToken :: IO (Maybe String)
getToken = do
    config <- readConfig configPath
    return $ token <$> config

main = do
    manager <- newManager tlsManagerSettings
    tokenMaybe <- getToken
    if isNothing tokenMaybe
        then print "Cannot load token from config"
    else do
        let token = fromJust tokenMaybe
        getMe token manager >>= print
