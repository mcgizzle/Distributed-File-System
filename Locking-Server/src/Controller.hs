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
{-# LANGUAGE RecordWildCards #-}

module Controller(
lockFile
)where

import Database.Persist
import Servant

import Models as M 
import Config

import Data.Maybe (fromJust)
import Control.Monad.Reader (when, MonadIO, MonadReader, asks, liftIO)

lockFile :: MonadIO m => Maybe FilePath -> MagicT m M.LockInfo
lockFile path' = do
  let path = fromJust path'
  isLocked <- checkIfLocked path
  case isLocked of
    True -> do
      liftIO $ putStrLn "File is locked adding client to queue"
      id <- addToQueue path
      return $ M.LockInfo path id False
    False -> do
      runDB $ insert $ M.LockQueue path [0]
      return $ M.LockInfo path 0 True

checkIfLocked :: MonadIO m => FilePath -> MagicT m Bool
checkIfLocked path = do
  res <- runDB $ selectFirst [ M.LockQueueFilePath ==.  path ] []
  case res of
    Just res' -> return True
    Nothing   -> return False

--addToQueue :: MonadIO m => MagicT m Int
addToQueue path = return 1
