{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Server
    ( startApp
    , app
    ) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
--import Data.List
import Data.Maybe (fromMaybe)
--import Data.String.Conversions
--import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp hiding (FileInfo)
import Servant
import System.Directory
import qualified Data.Aeson.Parser

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

type Nodes = [Int]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FileInfo json
    filename String
    filepath String
    UniqueFilepath filepath
    nodes Nodes 
    deriving Show
|]

connStr = "host=localhost dbname=test user=root password=root port=5432"

data File = File {
  
  name :: String,
  contents :: String

} deriving(Generic,Show)
instance ToJSON File
instance FromJSON File

type API = ReqBody '[JSON] File :> Post '[JSON] FileInfo 

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = putFile

putFile :: File -> Handler FileInfo
putFile f = do
  resp <- liftIO $ insertFile f
  case resp of
    Just resp' -> return resp'
    Nothing    -> throwError errFileExists
  where errFileExists = err404 { errBody = "This file already exists, please leave this server alone." }
  
  
insertFile f = runDB query
  where 
    query = do 
      resp <- insertUnique $ FileInfo fname fname [1]
      case resp of
        Just resp' -> get resp'
        Nothing    -> return Nothing 
    fname = name f

--runDB :: _ -> _ -> _
runDB query = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        query

