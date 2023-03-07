module ConfigReader where

import Text.JSON.Generic

expectedConfigClass = 1

data Config = Config
    {
        token :: String,
        configClassVersion :: Integer
    } deriving (Show, Data, Typeable)

readConfig :: String -> IO Config
readConfig path = do
    configSerialized <- readFile path
    let decodedJson = decodeJSON configSerialized
    return $ if configClassVersion decodedJson /= expectedConfigClass then error "Config schema is not up to date with code version" else decodedJson