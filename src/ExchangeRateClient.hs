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

data Market 
    {
        name :: String
    } deriving (Show, Generic, ToJSON, FromJSON)


get_currencies :: Manager -> IO (Maybe [Currency])
get_currencies manager = do
    request <- parseRequest "https://api.coingecko.com/api/v3/coins/list"
    response <- Network.HTTP.Client.httpLbs request manager
    let body = getResponseBody response
    let response = decode body 
    return response

get_rate :: Manager -> String -> IO (Maybe Rate)
get_rate manager id = do
    request <- parseRequest $ "https://api.coingecko.com/api/v3/coins/" ++ id ++ "/tickers"
    response <- Network.HTTP.Client.httpLbs request manager
    let body = getResponseBody response
    let response = decode body 
    return response
