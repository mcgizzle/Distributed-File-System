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
listFiles,newFile,writeFile,getFileLoc,
initFileNode
)where

import Prelude hiding (writeFile)

import Database.Persist
import Database.Persist.Sql
import Servant

import Data.Maybe (fromJust)
import Data.Time

import System.FilePath

import Config
import Database

import Api.Directory as M 
import Api.File

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO, lift)

---- File Nodes ------------------------
initFileNode ::  MonadIO m => String -> Int -> MagicT m InitResponse
initFileNode host port = do
  let entry = M.FileNode host port Nothing True
  runDB $ upsert entry [M.FileNodeActive =. True]
  return $ M.InitResponse True

getAvailableNodes :: MonadIO m => MagicT m ([M.FileNode],[Key M.FileNode])
getAvailableNodes = do
  time <- liftIO $ getCurrentTime
  res <- runDB $ do
          nodes <- selectList [M.FileNodeActive ==. True] [Desc M.FileNodeLastStore, LimitTo 3 ]
          mapM_ (\ node -> update (entityKey node) [M.FileNodeLastStore =. Just time]) nodes
          return nodes
  return (Prelude.map entityVal res, Prelude.map entityKey res)

nodeFromKey :: MonadIO m => M.FileNodeId -> MagicT m M.FileNode
nodeFromKey key = do
  res :: Maybe (Entity M.FileNode) <- runDB $ selectFirst [ M.FileNodeId ==. key ] []
  case res of
    Just res' -> return $ entityVal res'
    Nothing   -> throwError errNodeDoesNotExist

----------------------------------------

listFiles :: MonadIO m => MagicT m [FileInfo]
listFiles = do
  res :: [Entity M.FileInfo] <- runDB $ selectList [] []
  case res of
    []   -> throwError err404
    _    -> return $ Prelude.map entityVal res
  
getFileLoc :: MonadIO m => Maybe FilePath -> MagicT m [FileNode]
getFileLoc path' = do
  let path = takeDirectory $ fromJust path'
  let name = takeFileName path
  res <- runDB $ selectFirst [ M.FileInfoFile_path ==. path
                             , M.FileInfoFile_name ==. name ] []
  case res of
    Just res' -> do
            nodes <- mapM nodeFromKey (M.fileInfoNodes $ entityVal res')
            return nodes     
    Nothing   -> throwError errFileDoesNotExist

writeFile :: MonadIO m => File -> MagicT m FileInfo
writeFile f@File{..} = do
  res <- runDB $ selectFirst [ M.FileInfoFile_path ==. filePath
                             , M.FileInfoFile_name ==. fileName ] []
  case res of
    Just res' -> do
      let nodeKeys = M.fileInfoNodes $ entityVal res'
      nodes <- mapM nodeFromKey nodeKeys
      let fileKey = entityKey res'
      liftIO $ mapM (\n -> Api.File.query (updateFile' f)  (M.fileNodeHost n,M.fileNodePort n)) nodes
      time <- liftIO getCurrentTime
      let f' = FileInfo fileName filePath time nodeKeys
      runDB $ replace fileKey f'
      return f'
    Nothing   -> throwError errFileDoesNotExist

newFile :: MonadIO m => File -> MagicT m M.FileInfo
newFile f = do
  (nodes,keys) <- getAvailableNodes
  liftIO $ mapM_ (\n -> Api.File.query (sendFile' f) (M.fileNodeHost n,M.fileNodePort n)) nodes
  time <- liftIO getCurrentTime
  let f' = FileInfo (fileName f) (filePath f) time keys
  res <- runDB $ insertUnique f'
  case res of
    Just _  -> return f'
    Nothing -> throwError errFileExists


errFileDoesNotExist = err404 { errBody = "This file does not exits brah"}
errFileExists = err400 { errBody = "File with this name already exists"}
errNodeDoesNotExist = err404 { errBody = "Node with this ID does not exist"}
