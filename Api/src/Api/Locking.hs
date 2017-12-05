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

module Api.Locking where

import Servant
import Data.Aeson           (FromJSON, ToJSON)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)

import GHC.Generics         (Generic)

data LockInfo = LockInfo {
  path :: String,
  id :: Int,
  locked :: Bool
}deriving(Generic,Show)
instance ToJSON LockInfo
instance FromJSON LockInfo

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
LockQueue json
    filePath String
    queue [Int]
    UniqueFilePath filePath 
|]


type LockingAPI = "lock" :> Capture "id" Int :> 
                  QueryParam "path" FilePath :> Get '[JSON] LockInfo
             :<|> "unlock" :>  Capture "id" Int :> 
                  QueryParam "path" FilePath :> Get '[JSON] ()


