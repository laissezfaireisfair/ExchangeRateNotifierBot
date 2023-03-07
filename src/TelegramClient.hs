module TelegramClient where

import Network.HTTP.Client
import Text.JSON.Generic

_getRequestString :: String -> String -> String
_getRequestString token methodName = "https://api.telegram.org/bot" ++ token ++ "/" ++ methodName

data User = User
    { id :: Integer
    , is_bot :: Bool
    , first_name :: String
    , username :: String
    , can_join_groups :: Bool
    , can_read_all_group_messages :: Bool
    , supports_inline_quieries  :: Bool
    } deriving (Show, Data, Typeable)

--getMe :: String -> Manager -> IO (Maybe User)
getMe token manager = do
    request <- parseRequest $ _getRequestString token "getMe"
    response <- httpLbs request manager
    return response
    --return Nothing