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
import Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                             derivePersistField, share, sqlSettings)
import Servant.API
import Servant.Client
import GHC.Generics         (Generic)
import Data.Time
import Api.Models

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Nodes json
    host String
    port Int
    type NodeType
    UniqueNode host port
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
       "file-conf"   :> Capture "host" String :> Capture "port" Int :> Get '[JSON] FSConfig
-- :<|> "dir-conf"    :> Capture "host" String :> Capture "port" Int :> Get '[JSON] Configuration      
-- :<|> "lock-conf"   :> Capture "host" String :> Capture "port" Int :> Get '[JSON] Configuration      
  :<|> "client-conf" :>                                                Get '[JSON] ClientConfig 

fileConf' :: String -> Int -> ClientM FSConfig
--dirConf' :: String -> Int -> ClientM DirConfig 
--lockConf' :: String -> Int -> ClientM Configuration
clientConf' :: ClientM ClientConfig
--(fileConf' :<|> dirConf' :<|> lockConf' :<|> clientConf' ) = client configApi
(fileConf' :<|> clientConf') = client configApi
