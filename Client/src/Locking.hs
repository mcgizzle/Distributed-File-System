{-# LANGUAGE FlexibleContexts #-} 
module Locking where
import Control.Monad.Reader

import Api.Query
import Api.Locking

import Config

import Data.Maybe (fromJust)

lockingServer = ("localhost",9000)

checkLock :: FilePath -> App Bool
checkLock path = do
  res <- sendQuery (checkLock' (Just path))
  case res of
    Left _ -> return False
    Right _ -> return True

lockFile :: FilePath -> App ()
lockFile path = do
  id <- asks userId
  res <- sendQuery (lockFile' id (Just path))
  case res of
    Right res' -> liftIO $ putStrLn $ "You have succesfully locked the file.\n"
                         ++ "The lock will terminate on: "
                         ++ show (timeout res')
    Left err -> liftIO $ print err

unlockFile :: FilePath -> App ()
unlockFile path = do
  id <- asks userId
  res <- sendQuery (unlockFile' id (Just path))
  case res of
    Right _  -> liftIO $ putStrLn "The file has been unlocked."
    Left err -> liftIO $ print err


sendQuery q = do
  node <- asks lockingNode
  liftIO $ Api.Query.send q node
