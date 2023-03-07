{-# LANGUAGE DeriveDataTypeable #-}

module ConfigReader where

import Text.JSON.Generic ( Data, Typeable, decodeJSON )
import System.Directory ( doesFileExist )

expectedConfigClass = 1

data Config = Config
    {   token :: String
    ,   configClassVersion :: Integer
    } deriving (Show, Data, Typeable)

readConfig :: String -> IO (Maybe Config)
readConfig path = do
    isFileExists <- doesFileExist path
    if isFileExists then do
        configSerialized <- readFile path
        let decodedJson = decodeJSON configSerialized
        return $ if configClassVersion decodedJson /= expectedConfigClass then Nothing else Just decodedJson
    else return Nothing