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
lockFile,unlockFile
)where

import Database.Persist
import Servant

import Models as M 
import Config

import Data.List as DL
import Data.Maybe (fromJust)
import Control.Monad.Reader (when, MonadIO, MonadReader, asks, liftIO)

unlockFile :: MonadIO m => Int -> Maybe FilePath -> MagicT m ()
unlockFile id path' = do
  let path = fromJust path'
  isLocked <- checkIfLocked path
  if isLocked then do
    liftIO $ putStrLn $ "Unlocking file: " ++ path
    updateQueue (flip DL.delete) path id
    --alertNextInQueue path
    return ()
  else throwError errNoLock

lockFile :: MonadIO m => Int -> Maybe FilePath -> MagicT m M.LockInfo
lockFile id path' = do
  let path = fromJust path'
  isLocked <- checkIfLocked path
  if isLocked then do
    liftIO $ putStrLn $ "File: "++ path  ++" is locked adding client to queue"
    updateQueue (++) path [id]
    return $ M.LockInfo path id False
  else do
    liftIO $ putStrLn $ "Locking file: "++ path
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

updateQueue :: MonadIO m => ([Int] -> t -> [Int]) -> FilePath -> t -> MagicT m ()
updateQueue f path id = runDB $ do
  res <- selectFirst [ M.LockQueueFilePath ==. path ] []
  let q = lockQueueQueue $ entityVal $ fromJust res
  updateWhere [ M.LockQueueFilePath ==. path ] [ M.LockQueueQueue =. (f q id) ]

errNoLock = err404 { errBody = "User does not have a lock on this file"}
