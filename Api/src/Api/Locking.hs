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
import Servant.API
import Servant.Client
import GHC.Generics         (Generic)
import Data.Time

newtype AccessResponse = AccessResponse{
  response :: Bool
}deriving(Generic,Show)
instance ToJSON AccessResponse
instance FromJSON AccessResponse

data LockInfo = LockInfo {
  path :: String,
  id :: Int,
  locked :: Bool,
  timeout :: Maybe UTCTime
}deriving(Generic,Show)
instance ToJSON LockInfo
instance FromJSON LockInfo

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
LockQueue json
    filePath String
    queue [Int]
    timeout UTCTime 
    UniqueFilePath filePath 
|]
lockingApi :: Proxy LockingAPI
lockingApi = Proxy

type LockingAPI = QueryParam "path" FilePath :> Get '[JSON] AccessResponse 
             :<|> "lock" :> Capture "id" Int :> 
                  QueryParam "path" FilePath :> Get '[JSON] LockInfo
             :<|> "unlock" :>  Capture "id" Int :> 
                  QueryParam "path" FilePath :> Get '[JSON] ()

checkLock' :: Maybe FilePath -> ClientM AccessResponse
lockFile' :: Int -> Maybe FilePath -> ClientM LockInfo
unlockFile' :: Int -> Maybe FilePath -> ClientM ()
(checkLock' :<|> lockFile' :<|> unlockFile') = client lockingApi
