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
fileConf,clientConf
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
  runDB $ insert $ Nodes host port FileServer
  dirNode <- getNode DirectoryServer
  return $ FSConfig dirNode

clientConf :: MonadIO m => MagicT m ClientConfig
clientConf = do
  fileNode <- getNode FileServer
  dirNode <- getNode DirectoryServer
  lockNode <- getNode LockingServer
  return $ ClientConfig fileNode dirNode lockNode

getNode :: MonadIO m => NodeType -> MagicT m Node
getNode n = do
  res <- runDB $ selectList [NodesType ==. n] [LimitTo 1] 
  return $ (nodesHost $ entityVal $ head res, nodesPort $ entityVal $ head res)


