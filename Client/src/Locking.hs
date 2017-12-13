{-# LANGUAGE FlexibleContexts #-} 
module Locking where
import Control.Monad.Reader
import Control.Monad.Except

import Api.Query
import Api.Locking

import Config

import Data.Maybe (fromJust)

checkLock :: FilePath -> AppT () 
checkLock path = sendQuery (checkLock' (Just path)) "Uh oh! Looks like the file is locked." >> return ()

lockFile :: FilePath -> AppT ()
lockFile path = do
  id <- asks userId
  res <- sendQuery (lockFile' id (Just path)) "File is locked! you have been added to the queue."
  liftIO $ putStrLn $ "You have succesfully locked the file.\n"
                         ++ "The lock will terminate on: "
                         ++ show (timeout res)

unlockFile :: FilePath -> AppT ()
unlockFile path = do
  id <- asks userId
  sendQuery (unlockFile' id (Just path)) "Looks like you dont have a lock on this file."
  liftIO $ putStrLn "The file has been unlocked."


sendQuery q err = do
  node <- asks lockingNode
  res <- liftIO $ Api.Query.query q node
  case res of
    Left  _ -> throwError $ Error err
    Right res -> return res
