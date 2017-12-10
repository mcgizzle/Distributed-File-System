{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Controller(
fileConf,directoryConf,lockingConf,clientConf
)where

import Database.Persist
import Servant

import Config
import Database
import Api.Config
import Api.Models

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO, lift)

fileConf :: MonadIO m => String -> Int -> MagicT m FSConfig
fileConf host port = do
  insertNode FileServer host port 
  dirNode <- getNode DirectoryServer
  return $ FSConfig dirNode

clientConf :: MonadIO m => MagicT m ClientConfig
clientConf = do
  fileNode <- getNode FileServer
  dirNode <- getNode DirectoryServer
  lockNode <- getNode LockingServer
  return $ ClientConfig fileNode dirNode lockNode

lockingConf :: MonadIO m => String -> Int -> MagicT m ()
lockingConf = insertNode LockingServer 

directoryConf :: MonadIO m => String -> Int -> MagicT m ()
directoryConf = insertNode DirectoryServer

insertNode :: MonadIO m => NodeType -> String -> Int -> MagicT m ()
insertNode n host port = do
  runDB $ insert $ Nodes host port n
  return ()

getNode :: MonadIO m => NodeType -> MagicT m Node
getNode n = do
  res <- runDB $ selectList [NodesType ==. n] [LimitTo 1] 
  return (nodesHost $ entityVal $ head res, nodesPort $ entityVal $ head res)


