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
checkLock,lockFile,unlockFile
)where

import Database.Persist
import Servant

import Config
import Database

import Data.List as DL
import Data.Maybe (fromJust,isJust)
import Control.Monad.Reader (when, MonadIO, MonadReader, asks, liftIO, lift)

import Data.Time
import Data.Time.Clock

import Api.Locking as M

unlockFile :: MonadIO m => Int -> Maybe FilePath -> MagicT m ()
unlockFile id path' = do
  let path = fromJust path'
  liftIO $ putStrLn $ "Unlocking file: " ++ path
  updateQueue (flip DL.delete) path id
  return ()

lockFile :: MonadIO m => Int -> Maybe FilePath -> MagicT m M.LockInfo
lockFile id path' = do
  let path = fromJust path'
  gotLock <- isAvailable id path
  if (not gotLock) then do
    inQueue <- checkQueued path id
    if inQueue then throwError errUserInQueue
    else do
      liftIO $ putStrLn $ "File: "++ path  ++" is locked adding client to queue"
      updateQueue (++) path [id]
      return $ M.LockInfo path id False Nothing
  else do
    liftIO $ putStrLn $ "Locking file: "++ path
    now <- liftIO $ getCurrentTime
    runDB $ insertUnique $ M.LockQueue path [id] now
    return $ M.LockInfo path id True (Just now)

checkLock :: MonadIO m => Maybe FilePath -> MagicT m AccessResponse
checkLock path = do
  available <- isAvailable 0 (fromJust path)
  if available then return $ AccessResponse True
  else return $ AccessResponse False

isAvailable :: MonadIO m => Int -> FilePath -> MagicT m Bool
isAvailable id path = do
  res <- runDB $ do
    now <- liftIO $ getCurrentTime
    locked <- selectFirst [ M.LockQueueFilePath ==. path] []
    inDate <- selectFirst [ M.LockQueueFilePath ==. path
                                   , M.LockQueueTimeout <. now ] [] 
    case (isJust locked,isJust inDate) of
         (True,True)  -> return False
         (True,False) -> nextInQueue path id
         _            -> return True
  return res

nextInQueue path id = do
  res <- selectFirst [ M.LockQueueFilePath ==. path ] []
  let q = tail $ lockQueueQueue $ entityVal $ fromJust res
  now <- liftIO getCurrentTime
  let day = fromInteger 86400 :: NominalDiffTime
  let timeout = addUTCTime day now
  updateWhere [ M.LockQueueFilePath ==. path ] [ M.LockQueueQueue =. q
                                               , M.LockQueueTimeout =. timeout ]
  if (head q) == id then return True
  else return False 

checkQueued :: MonadIO m => String -> Int -> MagicT m Bool
checkQueued path id = do
  res <- runDB $ selectFirst [M.LockQueueFilePath ==. path] []
  return $ DL.elem id $ lockQueueQueue $ entityVal $ fromJust res

updateQueue :: MonadIO m => ([Int] -> t -> [Int]) -> FilePath -> t -> MagicT m ()
updateQueue f path id = runDB $ do
  res <- selectFirst [ M.LockQueueFilePath ==. path ] []
  let q = lockQueueQueue $ entityVal $ fromJust res
  updateWhere [ M.LockQueueFilePath ==. path ] [ M.LockQueueQueue =. (f q id) ]

errUserInQueue = err400 { errBody = "User is already in the queue, will be notified when the resource becomes available"}
errNoLock = err404 { errBody = "User does not have a lock on this file"}
