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
listFiles,newFile,writeFile,getFile
)where

import Prelude hiding (writeFile)

import Database.Persist
import Database.Persist.Sql
import Servant

import Data.Maybe (fromJust)
import Data.Time

import Config
import Database

import Api.Directory as M
import Api.File

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)

listFiles :: MonadIO m => MagicT m [FileInfo]
listFiles = do
  res :: [Entity M.FileInfo] <- runDB $ selectList [] []
  case res of
    []   -> throwError err404
    _    -> return $ Prelude.map entityVal res
  
getFile :: MonadIO m => Maybe FilePath -> Maybe String -> MagicT m FileInfo
getFile path name = do
  let path' = fromJust path
  let name' = fromJust name
  res <- runDB $ selectFirst [ M.FileInfoFile_path ==. path'
                             , M.FileInfoFile_name ==. name' ] []
  case res of
    Just res' -> return $ entityVal res'
    Nothing -> throwError errFileDoesNotExist

writeFile :: MonadIO m => File -> MagicT m FileInfo
writeFile f@File{..} = do
  res <- runDB $ selectFirst [ M.FileInfoFile_path ==. filePath
                             , M.FileInfoFile_name ==. fileName ] []
  case res of
    Just res' -> do
      let nodeKeys = M.fileInfoNodes $ entityVal res'
      nodes <- mapM nodeFromKey nodeKeys
      let fileKey = entityKey res'
      liftIO $ mapM (\n -> query (updateFile f)  n) nodes
      time <- liftIO getCurrentTime
      let f' = FileInfo fileName filePath time nodeKeys
      runDB $ replace fileKey f'
      return f'
    Nothing   -> throwError errFileDoesNotExist

newFile :: MonadIO m => File -> MagicT m FileInfo
newFile f = do
  (nodes,keys) <- getAvailableNodes
  liftIO $ mapM_ (\n -> query (sendFile f) n) nodes
  time <- liftIO getCurrentTime
  let f' = FileInfo (fileName f) (filePath f) time keys
  res <- runDB $ insertUnique f'
  case res of
    Just _  -> return f'
    Nothing -> throwError errFileExists

nodeFromKey :: MonadIO m => M.FileNodeId -> MagicT m M.FileNode
nodeFromKey key = do
  res :: Maybe (Entity M.FileNode) <- runDB $ selectFirst [ M.FileNodeId ==. key ] []
  case res of
    Just res' -> return $ entityVal res'
    Nothing   -> throwError errNodeDoesNotExist

getAvailableNodes :: MonadIO m => MagicT m ([M.FileNode],[Key M.FileNode])
getAvailableNodes = do
  res :: [Entity M.FileNode] <- runDB $ selectList [M.FileNodeActive ==. True] []
  return (Prelude.map entityVal res, Prelude.map entityKey res)

errFileDoesNotExist = err404 { errBody = "This file does not exits brah"}
errFileExists = err400 { errBody = "File with this name already exists"}
errNodeDoesNotExist = err404 { errBody = "Node with this ID does not exist"}
