{-# LANGUAGE DuplicateRecordFields #-}
module BotLogic where
import qualified TelegramClient as TGC
import Control.Monad.Trans.Maybe
import Text.Regex.Posix ( (=~) )
import qualified ExchangeRateClient as ERC
import Control.Monad.Cont
import Network.HTTP.Client (Manager)

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

currencyToString :: ERC.Currency -> [Char]
currencyToString c = id ++ " " ++ name ++ "\n"
                   where
                    id = ERC.id c 
                    name = (ERC.name :: ERC.Currency -> String) c

formatCurrencies :: [ERC.Currency] -> [String]
formatCurrencies currencies = group currencyStrings
                            where
                                currencyStrings = filter isGoodName $ map currencyToString currencies
                                isGoodName = all (\ c -> 'a' <= c && c <= 'z' || c == '\n' || c == ' ' || c == '-' || '0' <= c && c <= '9' || 'A' <= c && c <= 'Z')
                                group [] = []
                                group s = concat (take inOneMessage s) : group (drop inOneMessage s)
                                inOneMessage = 100

tickerToString :: ERC.Ticker -> String
tickerToString ticker = "On " ++ marketName ++ " in " ++ target ++ " is " ++ last ++ "\n"
                      where
                        target = ERC.target ticker
                        last = show $ ERC.last ticker
                        marketName = (ERC.name :: ERC.Market -> String) $ ERC.market ticker

formatRate :: ERC.Rate -> String
formatRate rate = "Rate of " ++ name ++ ":\n" ++ tickersInfo
                where
                    tickersInfo = concat tickersStrings
                    tickersStrings = map tickerToString $ ERC.tickers rate
                    name = (ERC.name :: ERC.Rate -> String) rate

executeCommand :: Manager -> Maybe Command -> IO [String]
executeCommand _ Nothing =  return ["Неизвестная команда либо неправильный параметр, используйте /help для получения списка команд"]
executeCommand _ (Just Help) = return ["/currencies - список валют (сперва код валюты, через пробел название)\n/rate *код валюты* - состояние валюты"]
executeCommand mgr (Just Currencies) = do
    currenciesMb <- ERC.getCurrencies mgr
    case currenciesMb of
        Just currencies -> return $ formatCurrencies currencies
        Nothing ->  return ["Не удается загрузить валюты"]
executeCommand mgr (Just (Rate name)) = do
    rateMb <- ERC.getRate mgr name
    print rateMb
    case rateMb of
        Just rate -> return [formatRate rate]
        Nothing ->  return ["Не удается загрузить курс"]

updateToReplies :: Manager -> TGC.Update -> MaybeT IO [TGC.MessageToSend]
updateToReplies  mgr u = do
    msg <- MaybeT . return $ TGC.message u
    txt <- MaybeT . return $ TGC.text msg
    sndr <- MaybeT . return $ TGC.from msg
    let chtId = TGC.id sndr
    let command = parseCommand txt
    answers <- lift $ executeCommand mgr command
    return $ map (\a -> TGC.MessageToSend {TGC.chat_id=show chtId, TGC.text_MTS=a}) answers

updatesToReplies :: Manager -> [TGC.Update] -> MaybeT IO [TGC.MessageToSend]
updatesToReplies manager updates = do
    updatesLists <- mapM (updateToReplies manager) updates
    return $ concat updatesLists

replyToMessages :: Manager -> Maybe [TGC.Update] -> MaybeT IO [TGC.MessageToSend]
replyToMessages manager updatesMb = do
    updates <- MaybeT . return $ updatesMb
    updatesToReplies manager updates