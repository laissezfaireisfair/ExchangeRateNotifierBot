import TelegramClient
import Network.HTTP.Client
import ConfigReader

configPath = "../config.json"

getToken :: IO String
getToken = do
    config <- readConfig configPath
    return $ token config

main = do
    manager <- newManager defaultManagerSettings
    token <- getToken
    response <- getMe token manager
    putStrLn ""
