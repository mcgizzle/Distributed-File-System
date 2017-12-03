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

unlockFile :: MonadIO m => Int -> Maybe FilePath -> MagicT m ()
unlockFile id path' = do
  let path = fromJust path'
  isLocked <- checkIfLocked path
  if isLocked then do
    liftIO $ putStrLn "Unlocking file"
    removeFromQueue path id
    alertNextInQueue path
    return ()
  else throwError errNoLock

lockFile :: MonadIO m => Int -> Maybe FilePath -> MagicT m M.LockInfo
lockFile id path' = do
  let path = fromJust path'
  isLocked <- checkIfLocked path
  case isLocked of
    True -> do
      liftIO $ putStrLn "File is locked adding client to queue"
      addToQueue path id
      return $ M.LockInfo path id False
    False -> do
      runDB $ insertUnique $ M.LockQueue path [id]
      return $ M.LockInfo path id True

checkIfLocked :: MonadIO m => FilePath -> MagicT m Bool
checkIfLocked path = do
  res <- runDB $ selectFirst [ M.LockQueueFilePath ==.  path ] []
  case res of
    Just res' -> return True
    Nothing   -> return False

-- TODO: Function to alert user when file becomes available
alertNextInQueue path = undefined 

removeFromQueue :: MonadIO m => FilePath -> Int -> MagicT m ()
removeFromQueue path id = runDB $ updateWhere [ M.LockQueueFilePath ==. path ]  
                                              [ M.LockQueueQueue -=. [id] ]


addToQueue :: MonadIO m => FilePath -> Int -> MagicT m ()
addToQueue path id = runDB $ updateWhere [ M.LockQueueFilePath ==. path ]  
                                         [ M.LockQueueQueue +=. [id] ]

errNoLock = err404 { errBody="User does not have a lock on this file"}
