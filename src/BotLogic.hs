module BotLogic where
import qualified TelegramClient as TGC
import Text.Regex.Posix ( (=~) )

data Command = Help | Currencies | Rate String

parseCommand :: String -> Maybe Command
parseCommand "/start" = Just Help
parseCommand "/help" = Just Help
parseCommand "/currencies" = Just Currencies
parseCommand text = if text =~ rateRegex then Just (Rate currency) else Nothing
                  where
                    currency = head parts !! 1
                    parts = text =~ rateRegex :: [[String]]
                    rateRegex = "^/rate +([a-zA-Z]+)$"

executeCommand :: Maybe Command -> String
executeCommand Nothing = "Неизвестная команда, используйте /help для получения списка команд"
executeCommand (Just Help) = "/currencies - список валют\n/rate *код валюты* - состояние валюты"
executeCommand (Just Currencies) = "Валюты:\nBTC\nETH"
executeCommand (Just (Rate name)) = "Текущий курс " ++ name ++ ": неизвесто"

updateToReply :: TGC.Update -> Maybe TGC.MessageToSend
updateToReply u = do
    msg <- TGC.message u
    txt <- TGC.text msg
    sndr <- TGC.from msg
    let chtId = TGC.id sndr
    let command = parseCommand txt
    let answer = executeCommand command
    return $ TGC.MessageToSend {TGC.chat_id=show chtId, TGC.text_MTS=answer}

replyToMessages :: Maybe [TGC.Update] -> Maybe [TGC.MessageToSend]
replyToMessages updatesMb = do
    updates <- updatesMb
    mapM updateToReply updates