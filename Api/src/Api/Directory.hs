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
import Data.Aeson

import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import GHC.Generics         (Generic)
import Data.Text            (Text)
import Data.Time

import Api.File

newtype InitResponse = InitResponse {
                  success :: Bool
                        } deriving(Generic)
instance ToJSON InitResponse
instance FromJSON InitResponse

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FileNode json
    host String
    port Int
    active Bool
FileInfo json
    file_name String
    file_path String
    last_write UTCTime
    nodes [FileNodeId]
    UniqueFile file_path file_name
    deriving Show
|]


type DirectoryAPI = "ls"                              :> Get '[JSON] [FileInfo]
               :<|> "new"   :> ReqBody '[JSON] File   :> Post '[JSON] FileInfo 
               :<|> "write" :> ReqBody '[JSON] File   :> Post '[JSON] FileInfo
               :<|> QueryParam "path" FilePath 
                 :> QueryParam "name" String          :> Get '[JSON] FileInfo  
               -- File server init endpoint
               :<|> "fs"    :> Capture "host" String  
                            :> Capture "port" Int     :> Post '[JSON] InitResponse 



