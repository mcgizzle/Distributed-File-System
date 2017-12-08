{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-} 

module Server
    ( runServer
    , app
    ) where

import Prelude ()
import Prelude.Compat

import Control.Monad (unless)
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
import System.FilePath

import Api.File hiding (api,updateFile)

runServer :: Int -> IO ()
runServer port = run port app

app :: Application
app = serve api server

api :: Proxy FileAPI
api = Proxy

server :: Server FileAPI
server = getFile :<|> putFile :<|> updateFile

putFile :: File -> Handler ServerInfo
putFile f@File{..} = do
  let path = "file-store" ++ filePath 
  unless (filePath == "/") $ liftIO $ createDirectoryIfMissing True path
  liftIO $ putStrLn $ "Saving file: "++ (path ++ fileName)
  exists <- liftIO $ doesFileExist (path ++ fileName)
  if exists
  then throwError errFileExists
  else do
    liftIO $ writeFile (path ++ fileName) fileContents 
    return $ ServerInfo True

getFile :: Maybe FilePath -> Handler File
getFile f' = do
  let f = fromJust f'
  let path = "file-store/" ++ f
  liftIO $ putStrLn $ "fetching file: "++ f
  exists <- liftIO $ doesFileExist path
  if exists
    then do
      contents <- liftIO $ readFile path 
      return $ File (takeFileName f) path contents
    else throwError errFileNotExists

updateFile :: File -> Handler ServerInfo
updateFile f@File{..} = do
  let path = "file-store" ++ filePath 
  liftIO $ putStrLn $ "updating file: "++ fileName
  liftIO $ writeFile (path ++ fileName) fileContents 
  return $ ServerInfo True


errFileNotExists= err404 { errBody = "File does not exist." }
errFileExists = err400 { errBody = "File with this name already exists"}

