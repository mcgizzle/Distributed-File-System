{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Api.Config where

import Servant
import Data.Aeson           (FromJSON, ToJSON)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH  (mkMigrate, mkPersist, mkDeleteCascade, 
                             persistLowerCase,derivePersistField, 
                             share, sqlSettings)
import Servant.API
import Servant.Client
import GHC.Generics         (Generic)
import Data.Time
import Api.Models
import Api.Query

configServerInfo = ("localhost",8000)

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Nodes json
    host String
    port Int
    type NodeType
    UniqueNodes host port
|]

type Node = (String,Int)


data FSConfig = FSConfig {
  fsDirServer :: Node
  }deriving(Generic)
instance ToJSON FSConfig
instance FromJSON FSConfig

data ClientConfig = ClientConfig {
  cFileServer :: Node,
  cDirServer  :: Node,
  cLockServer :: Node
  }deriving(Generic)
instance ToJSON ClientConfig
instance FromJSON ClientConfig


configApi :: Proxy ConfigAPI
configApi = Proxy

type ConfigAPI = 
       "file-conf"   :> Capture "host" String :> Capture "port" Int :> Post '[JSON] FSConfig
  :<|> "dir-conf"    :> Capture "host" String :> Capture "port" Int :> Post '[JSON] () 
  :<|> "lock-conf"   :> Capture "host" String :> Capture "port" Int :> Get '[JSON] ()      
  :<|> "client-conf" :>                                                Get '[JSON] ClientConfig 


fileConf' :: String -> Int -> ClientM FSConfig
dirConf' :: String -> Int -> ClientM () 
lockConf' :: String -> Int -> ClientM ()
clientConf' :: ClientM ClientConfig
(fileConf' :<|> dirConf' :<|> lockConf' :<|> clientConf') = client configApi

initDirServer host port  = query (dirConf' host port) configServerInfo
initLockServer host port = query (lockConf' host port) configServerInfo
initFileServer host port = query (fileConf' host port) configServerInfo
initClientServer         = query clientConf' configServerInfo
