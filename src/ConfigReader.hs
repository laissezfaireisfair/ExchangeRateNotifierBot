{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module ConfigReader where

import Data.Aeson ( FromJSON, ToJSON, decode )
import GHC.Generics ( Generic )
import System.Directory ( doesFileExist )
import qualified Data.ByteString.Char8 as S8

expectedConfigClass = 1

data Config = Config
    {   token :: String
    ,   configClassVersion :: Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

readConfig :: String -> IO (Maybe Config)
readConfig path = do
    isFileExists <- doesFileExist path
    if isFileExists then do
        configSerialized <- readFile path
        let configSerializedLazy = S8.fromStrict $ S8.pack configSerialized
        let decodedJson = decode configSerializedLazy
        let isCorrect = maybe False (\ j -> configClassVersion j == expectedConfigClass) decodedJson
        return $ if isCorrect then decodedJson else Nothing
    else return Nothing