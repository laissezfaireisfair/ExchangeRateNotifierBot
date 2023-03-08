{-# LANGUAGE DeriveDataTypeable #-}
module TelegramClient where

import Network.HTTP.Client ( httpLbs, parseRequest, Manager )
import Text.JSON.Generic ( Data, Typeable, decodeJSON )
import Network.HTTP.Simple ( getResponseBody )
import qualified Data.ByteString.Char8 as S8

_getRequestString :: String -> String -> String
_getRequestString token methodName = "https://api.telegram.org/bot" ++ token ++ "/" ++ methodName

data User = User
    { id :: Integer
    , is_bot :: Bool
    , first_name :: String
    , username :: String
    , can_join_groups :: Bool
    , can_read_all_group_messages :: Bool
    } deriving (Show, Data, Typeable)

data ResponseBody a = GetMeResponse
    {   ok :: Bool
    ,   result :: a
    } deriving (Show, Data, Typeable)

-- TODO: Handle request error
runRequest :: (Show a, Data a, Typeable a) => String -> Manager -> String -> IO (Maybe a)
runRequest token manager name = do
    request <- parseRequest $ _getRequestString token name
    response <- Network.HTTP.Client.httpLbs request manager
    let body = S8.unpack $ S8.toStrict $ getResponseBody response
    let response = decodeJSON body
    return $ if ok response then Just (result response) else Nothing

getMe :: String -> Manager -> IO (Maybe User)
getMe token manager = runRequest token manager "getMe"