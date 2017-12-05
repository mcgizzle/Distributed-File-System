{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Api.File where

import Api.Directory

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
          :<|> "write" :> ReqBody '[JSON] File :> Post '[JSON] ServerInfo

getFile' :<|> sendFile' :<|> updateFile' = client api

sendFile :: File -> ClientM ServerInfo
sendFile f = sendFile' f >>= return

updateFile :: File -> ClientM ServerInfo
updateFile f = updateFile' f

query :: Show a => ClientM a -> FileNode -> IO ()
query q node = do
  manager' <- newManager defaultManagerSettings
  let host = fileNodeHost node 
  let port = fileNodePort node
  res <- runClientM q (ClientEnv manager' (BaseUrl Http host port "")) 
  case res of
    Left err   -> print err
    Right res' -> print res'
