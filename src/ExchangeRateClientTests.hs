module ExchangeRateClientTests (runRateClientTests) where

import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Data.Maybe ( fromJust, isNothing )

import qualified ExchangeRateClient as ERC

-- Public
runRateClientTests :: Maybe String -> IO ()
runRateClientTests tokenMaybe = do
    putStrLn "Testing rate provider..."
    manager <- newManager tlsManagerSettings
    if isNothing tokenMaybe
        then putStrLn "Cannot load token from config"
    else do
        let token = fromJust tokenMaybe
        currenciesMb <- ERC.getCurrencies manager
        case currenciesMb of
            Just _ -> putStrLn "Get currencies - OK"
            Nothing -> putStrLn "Get currencies - FAILED"
        rateMb <- ERC.getRate manager "bitcoin"
        case rateMb of
            Just _ -> putStrLn "Get rate - OK"
            Nothing -> putStrLn "Get rate - FAILED"
-- /Public
