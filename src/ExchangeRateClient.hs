{-# LANGUAGE DuplicateRecordFields, DeriveGeneric, DeriveAnyClass #-}
module ExchangeRateClient where 

import Network.HTTP.Client ( httpLbs, parseRequest, Manager )
import Data.Aeson ( FromJSON, ToJSON, decode )
import GHC.Generics ( Generic )
import Network.HTTP.Simple ( getResponseBody )

data Currency = Currency
    { id :: String
    , name :: String
    } deriving (Show, Generic, ToJSON, FromJSON)

data Rate = Rate 
    { name :: String,
      tickers :: [Ticker]
    } deriving (Show, Generic, ToJSON, FromJSON)

data Ticker = Ticker
    { 
        target :: String,
        market :: Market,
        last :: Double
    } deriving (Show, Generic, ToJSON, FromJSON)

data Market = Market
    {
        name :: String
    } deriving (Show, Generic, ToJSON, FromJSON)


getCurrencies :: Manager -> IO (Maybe [Currency])
getCurrencies manager = do
    request <- parseRequest "https://api.coingecko.com/api/v3/coins/list"
    response <- Network.HTTP.Client.httpLbs request manager
    let body = getResponseBody response
    let response = decode body 
    return response

getRate :: Manager -> String -> IO (Maybe Rate)
getRate manager id = do
    request <- parseRequest $ "https://api.coingecko.com/api/v3/coins/" ++ id ++ "/tickers"
    response <- Network.HTTP.Client.httpLbs request manager
    let body = getResponseBody response
    let response = decode body 
    return response
