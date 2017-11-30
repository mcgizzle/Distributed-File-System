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
listFiles,putFile
)where

import Database.Persist
import Servant

import Models as M 
import Config
import FileApi

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)

listFiles :: MonadIO m => MagicT m [FileInfo]
listFiles = do
  res :: [Entity M.FileInfo] <- runDB $ selectList [] []
  case res of
    []   -> throwError err404
    _    -> return $ Prelude.map entityVal res
  
getFileLoc :: MonadIO m => String -> MagicT m FileInfo
getFileLoc path = do
  res <- runDB $ selectFirst [ M.FileInfoFile_path ==. path ] []
  case res of
    Nothing     -> throwError err404
    Just res'   -> return $ entityVal res'
    
putFile :: MonadIO m => File -> MagicT m FileInfo
putFile f = do
  (servers,keys) <- getServers
  liftIO $ mapM_ (\s -> query (sendFile f) s) servers
  let f' = FileInfo (fileName f) (filePath f) keys
  res <- runDB $ insertUnique f'
  case res of
    Just _  -> return f'
    Nothing -> throwError errFileExists

getServers :: MonadIO m => MagicT m ([M.FileNode],[Key M.FileNode])
getServers = do
  res :: [Entity M.FileNode] <- runDB $ selectList [M.FileNodeActive ==. True] []
  return $ (Prelude.map entityVal res, Prelude.map entityKey res)

errFileExists = err400 { errBody = "File with this name already exists"}
