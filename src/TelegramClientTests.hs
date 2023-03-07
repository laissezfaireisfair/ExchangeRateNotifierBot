import TelegramClient
import Network.HTTP.Client
import ConfigReader
import Network.HTTP.Client.TLS

configPath = "../config.json"

getToken :: IO String
getToken = do
    config <- readConfig configPath
    return $ token config
main = do
    manager <- newManager tlsManagerSettings
    token <- getToken
    response <- getMe token manager
    print response
