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

module Main where

import Server
import System.Environment
import Control.Monad (unless)

import Control.Monad.Reader

import Config
import Api.Directory
import Api.Query
import Api.Config

informNetwork :: Int -> AppT ()
informNetwork port = loop
  where 
    loop = do
      (dirHost,dirPort) <- asks fsDirServer
      resp <- liftIO $ queryBool (initFileNode' "localhost" port) (dirHost,dirPort) 
      unless resp loop
      return ()

startServer :: Int -> AppT ()
startServer port = do
  informNetwork port 
  liftIO $ putStrLn $ "Server is running on port: " ++ show port
  informNetwork port 
  liftIO $ putStrLn "Initialised with Directory Server"
  liftIO $ runServer port


main :: IO ()
main = do
  [port] <- getArgs
  res <- getConfig $ read port
  case res of
    Right cfg -> runReaderT (startServer $ read port) cfg
    Left err  -> putStrLn "There was an error connecting to the Config-Server: Aborting."
  
           
