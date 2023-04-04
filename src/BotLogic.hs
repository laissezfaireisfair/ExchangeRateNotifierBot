{-# LANGUAGE DuplicateRecordFields #-}

module BotLogic (replyToMessages) where

import Control.Monad.Trans.Maybe ( MaybeT(MaybeT) )
import Text.Regex.Posix ( (=~) )
import Control.Monad.Cont ( MonadTrans(lift) )
import Network.HTTP.Client (Manager)
import Data.Maybe (fromMaybe)
import Data.List ( find, isPrefixOf, tails )

import qualified ExchangeRateClient as ERC
import qualified TelegramClient as TGC

-- Public
replyToMessages :: Manager -> Maybe [TGC.Update] -> MaybeT IO [TGC.MessageToSend]
replyToMessages manager updatesMb = do
    updates <- MaybeT . return $ updatesMb
    updatesToReplies manager updates
-- /Public


-- Private
data Command = Help | Currencies | Rate String | SearchCurrency String

parseCommand :: String -> Maybe Command
parseCommand "/start" = Just Help
parseCommand "/help" = Just Help
parseCommand "/currencies" = Just Currencies
parseCommand text | text =~ rateRegex = Just (Rate currency)
                  where
                    currency = head parts !! 1
                    parts = text =~ rateRegex :: [[String]]
                    rateRegex = "^/rate +([0-9a-zA-Z-]+)$"
parseCommand text | text =~ rateRegex = Just (SearchCurrency name)
                  where
                    name = head parts !! 1
                    parts = text =~ rateRegex :: [[String]]
                    rateRegex = "^/search +([0-9a-zA-Z-]+)$"
parseCommand _ = Nothing

currencyToString :: ERC.Currency -> [Char]
currencyToString c = id ++ " " ++ name ++ "\n"
                   where
                    id = ERC.id c 
                    name = (ERC.name :: ERC.Currency -> String) c

isGoodStringToSend :: String -> Bool
isGoodStringToSend = all isGoodSymbol
                   where
                    isGoodSymbol c | 'a' <= c && c <= 'z' = True
                                   | 'A' <= c && c <= 'Z' = True
                                   | '0' <= c && c <= '9' = True
                                   | c == ' ' = True
                                   | c == '\n' = True
                                   | c == '-' = True
                                   | c == '.' = True
                                   | otherwise = False

formatCurrencies :: [ERC.Currency] -> [String]
formatCurrencies currencies = group currencyStrings
                            where
                                currencyStrings = filter isGoodStringToSend $ map currencyToString currencies
                                group [] = []
                                group s = concat (take inOneMessage s) : group (drop inOneMessage s)
                                inOneMessage = 100

tickerToString :: ERC.Ticker -> String
tickerToString ticker = marketName ++ " - " ++ last ++ " " ++ target ++ "\n"
                      where
                        target = ERC.target ticker
                        last = show $ ERC.last ticker
                        marketName = (ERC.name :: ERC.Market -> String) $ ERC.market ticker

formatRate :: ERC.Rate -> String
formatRate rate = "Rate of " ++ name ++ ":\n" ++ tickersInfo
                where
                    tickersInfo = concat $ filter isGoodStringToSend tickersStrings
                    tickersStrings = map tickerToString $ ERC.tickers rate
                    name = (ERC.name :: ERC.Rate -> String) rate

helpString :: String
helpString = "/search *название* - поиск валюты (возвращает код и название)\n"
           ++"/currencies - список валют (сперва код валюты, через пробел название)\n"
           ++"/rate *код валюты* - состояние валюты"

findString :: (Eq a) => [a] -> [a] -> Bool
findString search str = any (isPrefixOf search) (tails str)

searchCurrency :: String -> [ERC.Currency] -> String
searchCurrency name currencies = if null currencyStrings then "Валюта не найдена" else concat currencyStrings
                               where
                                currencyStrings =  filter (findString name) $ map currencyToString currencies

executeCommand :: Manager -> Maybe Command -> IO [String]
executeCommand _ Nothing =  return ["Неизвестная команда либо неправильный параметр, используйте /help для получения списка команд"]
executeCommand _ (Just Help) = return [helpString]
executeCommand mgr (Just Currencies) = do
    currenciesMb <- ERC.getCurrencies mgr
    case currenciesMb of
        Just currencies -> return $ formatCurrencies currencies
        Nothing ->  return ["Не удается загрузить валюты"]
executeCommand mgr (Just (Rate name)) = do
    rateMb <- ERC.getRate mgr name
    case rateMb of
        Just rate -> return [formatRate rate]
        Nothing ->  return ["Не удается загрузить курс"]
executeCommand mgr (Just (SearchCurrency name)) = do
    currenciesMb <- ERC.getCurrencies mgr
    case currenciesMb of
        Just currencies -> return [searchCurrency name currencies]
        Nothing ->  return ["Не удается загрузить валюты"]


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
-- /Private
