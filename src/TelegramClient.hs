{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module TelegramClient where

import Network.HTTP.Client ( httpLbs, parseRequest, Manager )
import Data.Aeson ( FromJSON, ToJSON, decode )
import GHC.Generics ( Generic )
import Network.HTTP.Simple ( getResponseBody )
import qualified Data.ByteString.Char8 as S8

_paramsToUrlQuery :: [(String, String)] -> String
_paramsToUrlQuery [] = ""
_paramsToUrlQuery params = "?" ++ foldr (\(n,v) acc -> "&" ++ n ++ "=" ++ v ++ acc) "" params

_getRequestString :: String -> String -> [(String, String)] -> String
_getRequestString token methodName params = "https://api.telegram.org/bot" ++ token ++ "/" ++ methodName ++ _paramsToUrlQuery params

data User = User
    { id :: Integer
    , is_bot :: Bool
    , first_name :: String
    , username :: String
    , can_join_groups :: Bool
    , can_read_all_group_messages :: Bool
    } deriving (Show, Generic, ToJSON, FromJSON)

data Message = Message
    { message_id :: Integer
    , text :: Maybe String
    } deriving (Show, Generic, ToJSON, FromJSON)

data Update = Update
    { update_id :: Integer
    , message :: Message
    } deriving (Show, Generic, ToJSON, FromJSON)

data ResponseBody a = ResponseBody
    {   ok :: Bool
    ,   result :: a
    } deriving (Show, Generic, ToJSON, FromJSON)

runRequest :: (Show a, Generic a, ToJSON a, FromJSON a) => String -> Manager -> String -> [(String, String)] -> IO (Maybe a)
runRequest token manager name params = do
    request <- parseRequest $ _getRequestString token name params
    response <- Network.HTTP.Client.httpLbs request manager
    let body = getResponseBody response
    let response = decode body
    let isCorrect = maybe False ok response
    return $ if isCorrect then fmap result response else Nothing

getMe :: String -> Manager -> IO (Maybe User)
getMe token manager = runRequest token manager "getMe" []

getMessageUpdates :: String -> Manager -> IO (Maybe [Update])
getMessageUpdates token manager = runRequest token manager "getUpdates" [("allowed_updates", "messages")]
