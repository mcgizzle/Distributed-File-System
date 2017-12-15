{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Api.Directory where

import Servant
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)

import Data.Aeson

import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import GHC.Generics         (Generic)
import Data.Text            (Text)
import Data.Time

import Api.File (File) 

newtype InitResponse = InitResponse {
                  success :: Bool
                        } deriving(Generic)
instance ToJSON InitResponse
instance FromJSON InitResponse

data CacheResponse = InDate | OutDate 
  deriving(Eq,Generic)
instance ToJSON CacheResponse
instance FromJSON CacheResponse

type Clients = (String,Int)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FileNode json
    host String
    port Int
    lastStore UTCTime Maybe
    active Bool
    UniqueNode host port
FileInfo json
    file_name String
    file_path String
    last_write UTCTime
    nodes [FileNodeId]
    UniqueFile file_path file_name
    deriving Show
FileCache json
    file_path String
    client Clients
|]




directoryApi :: Proxy DirectoryAPI
directoryApi = Proxy

type DirectoryAPI = 
       "ls"                                  :> Get '[JSON] [FileInfo]
  :<|> "new"   :> ReqBody '[JSON] File       :> Post '[JSON] FileInfo 
  :<|> "write" :> ReqBody '[JSON] File       :> Post '[JSON] FileInfo
  :<|> QueryParam "path" FilePath            :> Get '[JSON] [FileNode]  
  :<|> QueryParam "path" FilePath            :> Delete '[JSON] ()  
  :<|> "cache" :> QueryParam "path" FilePath  
               :> QueryParam "time" UTCTime  :> Get '[JSON] CacheResponse
               -- File server init endpoint
  :<|> "fs"    :> Capture "host" String  
               :> Capture "port" Int         :> Post '[JSON] InitResponse 
  :<|> "new"   :> Capture "host" String  
               :> Capture "port" Int   
               :> ReqBody '[JSON] File       :> Post '[JSON] FileInfo 
  :<|> "write" :> Capture "host" String  
               :> Capture "port" Int   
               :> ReqBody '[JSON] File       :> Post '[JSON] FileInfo


listFiles' :: ClientM [FileInfo]
newFile' :: File -> ClientM FileInfo
writeFile' :: File -> ClientM FileInfo
getFileLoc' :: Maybe String -> ClientM [FileNode]
removeFile' :: Maybe String -> ClientM ()
checkCache' :: Maybe String -> Maybe UTCTime -> ClientM CacheResponse
initFileNode' :: String -> Int -> ClientM InitResponse
-- Caching Version 2 endpoints
newFile'' :: String -> Int -> File -> ClientM FileInfo
writeFile'' :: String -> Int -> File -> ClientM FileInfo
( listFiles' :<|> newFile' :<|> writeFile' :<|> getFileLoc' :<|> 
  removeFile' :<|> checkCache' :<|> initFileNode' :<|> 
  newFile'' :<|> writeFile'' ) = client directoryApi

