{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module DirectoryServer
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
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import qualified Data.Aeson.Parser

data FileInfo = FileInfo {
  
  fileName :: String,
  filePath :: String,
  nodeId :: [Int]
  
}deriving (Generic)

instance of ToJSON File
instance of FromJSON File

type API = Capture "path" String :> Get '[JSON] FileInfo 
      :<|> "list" :> Get '[JSON] [FileInfo] 

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = getLoc :<|> getList

getLoc :: String -> Handler FileInfo
getLoc path = do

