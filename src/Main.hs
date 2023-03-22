import TelegramClientTests (runTelegramTests)
import Control.Monad (forever, unless)
import qualified TelegramClient as TGC
import qualified ConfigReader as CR
import Data.Maybe (isNothing, fromJust)
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad.State ( forever, unless, StateT, MonadTrans (lift), MonadState (get, put), evalStateT )
import Control.Concurrent (threadDelay)
import TelegramClient (Update(update_id))
import qualified BotLogic as BL

configPath :: String
configPath = "config.json"

getToken :: IO (Maybe String)
getToken = do
    config <- CR.readConfig configPath
    return $ CR.token <$> config

getLastUpdate :: Maybe Integer -> Maybe [TGC.Update] -> Maybe Integer
getLastUpdate last updatesMb = do
    updates <- updatesMb
    foldr (\ u acc -> (if isNothing acc || (update_id u >= fromJust acc) then Just (update_id u + 1) else acc)) last updates 

mainLoopIteration :: String -> Manager -> StateT (Maybe Integer) IO ()
mainLoopIteration token manager = do
    lift $ threadDelay 1000000
    lastUpdate <- get
    newUpd <- lift $ do
        newMessages <- TGC.getMessageUpdates token manager lastUpdate
        let newUpdate = getLastUpdate lastUpdate newMessages
        let messagesToSend = BL.replyToMessages newMessages
        unless (isNothing messagesToSend) (do
                mapM_ (TGC.sendMessage token manager) (fromJust messagesToSend)
            )
        return newUpdate
    put newUpd
    mainLoopIteration token manager

main :: IO ()
main = do
    tokenMaybe <- getToken
    runTelegramTests tokenMaybe
    unless (isNothing tokenMaybe) (do
        manager <- newManager tlsManagerSettings
        evalStateT (mainLoopIteration (fromJust tokenMaybe) manager) Nothing
        )
        