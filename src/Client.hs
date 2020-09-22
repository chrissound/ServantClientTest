{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Servant.Types.SourceT (foreach)
import Control.Monad.Free
import Servant.Client.Free

import qualified Servant.Client.Internal.HttpClient as I
import qualified Network.HTTP.Client                as HTTP

import qualified Servant.Client.Streaming as S

newtype HelloMessage = HelloMessage { title :: String }
  deriving (Show, Generic)

instance FromJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving (Show,Generic)

instance ToJSON ClientInfo


type API = "square"
  :> Capture "n" Int
  :> ReqBody '[JSON] ClientInfo
  :> Get '[JSON] Int

api :: Proxy API
api = Proxy

myapitest = I.client api

getSquare :: Int -> ClientInfo -> Free ClientF Int
getSquare = Servant.Client.Free.client api

test :: IO ()
test = case getSquare 12 (ClientInfo "" "" 123 []) of
    Pure n ->
        putStrLn $ "ERROR: got pure result: " ++ show n
    Free (Throw err) ->
        putStrLn $ "ERROR: got error right away: " ++ show err
    Free (RunRequest req k) -> do
      burl <- parseBaseUrl "http://localhost:8000"
      mgr <- HTTP.newManager HTTP.defaultManagerSettings
      let req' = I.requestToClientRequest burl req
      putStrLn $ "Making request: " ++ show req'
