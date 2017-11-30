{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module FileApi where

import Models (File(..),FileServer(..))

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

newtype ServerInfo = ServerInfo  {
  
  fileSaved :: Bool

} deriving(Generic,Show)
instance ToJSON ServerInfo
instance FromJSON ServerInfo

api :: Proxy FileAPI
api = Proxy

type FileAPI = Capture "path" String :> Get '[JSON] File 
          :<|> ReqBody '[JSON] File :> Post '[JSON] ServerInfo

getFile' :<|> sendFile' = client api

sendFile :: File -> ClientM ServerInfo
sendFile f = sendFile' f >>= return

query :: Show a => ClientM a -> FileServer -> IO ()
query q server = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM q (ClientEnv manager' (BaseUrl Http h p "")) 
  case res of
    Left err   -> print err
    Right res' -> print res'
  where 
    p = 8080
    h ="localhost"
