{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Api.File where

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

data File = File {
  fileName :: String,
  filePath :: String,
  fileContents :: String
 }deriving (Generic,Show)
instance FromJSON File
instance ToJSON File


fileApi :: Proxy FileAPI
fileApi = Proxy


type FileAPI = QueryParam "path" String :> Get '[JSON] File 
          :<|> ReqBody '[JSON] File :> Post '[JSON] ServerInfo
          :<|> "write" :> ReqBody '[JSON] File :> Post '[JSON] ServerInfo

getFile' :: Maybe String -> ClientM File
sendFile' :: File -> ClientM ServerInfo
updateFile' :: File -> ClientM ServerInfo
getFile' :<|> sendFile' :<|> updateFile' = client fileApi

query :: Show a => ClientM a -> (String, Int) -> IO ()
query q (host,port) = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM q (ClientEnv manager' (BaseUrl Http host port "")) 
  case res of
    Left err   -> print err
    Right res' -> print res'
