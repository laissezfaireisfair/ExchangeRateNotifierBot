import TelegramClient
import Network.HTTP.Client

-- WARNING: DO NOT COMMIT SENSITIVE DATA
token = undefined

main = do
    manager <- newManager defaultManagerSettings
    response <- getMe token manager
    putStrLn ""
