module Main where

import Client
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Servant.Types.SourceT (foreach)
import Text.Pretty.Simple

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  run
  test

queries = myapitest 10 (ClientInfo "" "" 123 [])

run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM queries (mkClientEnv manager' (BaseUrl Http "localhost" 8000 ""))
  case res of
    Right (message) -> do
      pPrint message
    Left err -> do
      putStrLn $ "Error: "
      pPrint err
