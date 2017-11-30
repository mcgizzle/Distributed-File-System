{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server
    ( runServer
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

import Api


runServer :: Int -> IO ()
runServer port = run port app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = getFile :<|> putFile

putFile :: File -> Handler ServerInfo
putFile f = do
  let path = "file-store/" ++ fileName f
  liftIO $ putStrLn $ "Saving file: "++ path
  exists <- liftIO $ doesFileExist path
  if exists
  then throwError errFileExists
  else do
    liftIO $ writeFile path (fileContents f)
    return $ ServerInfo True

getFile :: String -> Handler File
getFile f' = do
  let f = "file-store/" ++ f'
  liftIO $ putStrLn $ "fetching file: "++ f
  exists <- liftIO $ doesFileExist f
  if exists
    then do
      contents <- liftIO $ readFile f 
      return $ File f contents
    else throwError errFileNotExists

errFileNotExists= err404 { errBody = "File does not exist." }
errFileExists = err400 { errBody = "File with this name already exists"}

