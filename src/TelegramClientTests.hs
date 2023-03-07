import TelegramClient
import Network.HTTP.Client
import ConfigReader
import Network.HTTP.Client.TLS

configPath = "../config.json"

getToken :: IO String
getToken = do
    config <- readConfig configPath
    return $ token config

runGetMeTest :: String -> Manager -> IO  (Maybe User)
runGetMeTest = getMe

main = do
    manager <- newManager tlsManagerSettings
    token <- getToken
    runGetMeTest token manager >>= print
