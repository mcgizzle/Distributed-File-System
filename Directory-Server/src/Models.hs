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

module Models where

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Data.Aeson           (FromJSON, ToJSON)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import GHC.Generics         (Generic)
import Config               
import Data.Text            (Text)

data File = File {
  fileName :: String,
  filePath :: String,
  fileContents :: String
}deriving (Generic, Show)
instance FromJSON File
instance ToJSON File


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FileNode json
    host String
    port Int
    active Bool
FileInfo json
    file_name String
    file_path String
    nodes [FileNodeId]
    UniqueFile_path file_path
    deriving Show
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDB :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDB query = do
  p <- asks pool
  liftIO $ runSqlPool query p
