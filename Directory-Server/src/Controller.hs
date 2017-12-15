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
checkCache,newFileC,writeFileC,
removeFile,
initFileNode
)where

import Prelude hiding (writeFile)

import Database.Persist
import Database.Persist.Sql
import Servant

import Data.Maybe (fromJust)
import Data.Time

import System.FilePath hiding (splitPath)

import Config
import Database

import Api.Directory as M 
import Api.File
import Api.Query

import Control.Monad.Reader 

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

splitPath :: String -> (String,String)
splitPath path' = (name,path)
  where
    name = takeFileName path'
    path = takeDirectory path' ++ "/"
---------------------------------------
listFiles :: MonadIO m => MagicT m [FileInfo]
listFiles = do
  res :: [Entity M.FileInfo] <- runDB $ selectList [] []
  case res of
    []   -> throwError err404
    _    -> return $ Prelude.map entityVal res
  
getFileLoc :: MonadIO m => Maybe FilePath -> MagicT m [FileNode]
getFileLoc path' = do
  let (name,path) = splitPath $ fromJust path'
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
      liftIO $ mapM (\n -> Api.Query.query (updateFile' f)  (M.fileNodeHost n,M.fileNodePort n)) nodes
      time <- liftIO getCurrentTime
      let f' = FileInfo fileName filePath time nodeKeys
      runDB $ replace fileKey f'
      return f'
    Nothing   -> throwError errFileDoesNotExist

newFile :: MonadIO m => File -> MagicT m M.FileInfo
newFile f = do
  (nodes,keys) <- getAvailableNodes
  liftIO $ mapM_ (\n -> Api.Query.query (sendFile' f) (M.fileNodeHost n,M.fileNodePort n)) nodes
  time <- liftIO getCurrentTime
  let f' = FileInfo (fileName f) (filePath f) time keys
  res <- runDB $ insertUnique f'
  case res of
    Just _  -> return f'
    Nothing -> throwError errFileExists

removeFile :: MonadIO m => Maybe FilePath -> MagicT m ()
removeFile path' = do
  let (name,path) = splitPath $ fromJust path'
  res <- runDB $ do
    f' <- selectFirst [ M.FileInfoFile_path ==. path, M.FileInfoFile_name ==. name ] []
    case f' of
      Nothing -> return Nothing
      Just f  -> do
        delete $ entityKey f
        let nodeKeys = M.fileInfoNodes $ entityVal f
        return $ Just nodeKeys
  case res of
    Just keys -> do
        nodes <- mapM nodeFromKey keys
        liftIO $ mapM_ (\n -> Api.Query.query (deleteFile' path') (M.fileNodeHost n,M.fileNodePort n)) nodes
        return ()
    Nothing   -> throwError err404  

checkCache :: MonadIO m => Maybe String -> Maybe UTCTime -> MagicT m M.CacheResponse
checkCache path' time' = do
  res <- runDB $ selectFirst [ M.FileInfoFile_path ==. path,
                               M.FileInfoFile_name ==. name,
                               M.FileInfoLast_write <=. time ] []
  case res of 
    Just _  -> return InDate
    Nothing -> return OutDate
  where
    time = fromJust time'
    path = takeDirectory $ fromJust path'
    name = takeFileName $ fromJust path'

newFileC :: MonadIO m => String -> Int -> File -> MagicT m FileInfo
newFileC host port file@File{..} = do
  res <- runDB $ insert $ FileCache filePath (host,port)
  newFile file

writeFileC :: MonadIO m => String -> Int -> File -> MagicT m FileInfo
writeFileC host port file@File{..} = do
  runDB $ do
    res <- selectList [M.FileCacheFile_path ==. filePath ] []
    insert $ FileCache filePath (host,port)
    mapM_ delete (map entityKey res)
    liftIO $ mapM_ (\n -> Api.Query.query (sendFile' file) n) (map  (fileCacheClient . entityVal) res)
  writeFile file

errFileDoesNotExist = err404 { errBody = "This file does not exits brah"}
errFileExists = err400 { errBody = "File with this name already exists"}
errNodeDoesNotExist = err404 { errBody = "Node with this ID does not exist"}
