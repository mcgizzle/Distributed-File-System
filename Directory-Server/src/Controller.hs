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
  servers <- getServers
  liftIO $ mapM_ (query (sendFile f)) servers
  let f' = FileInfo (fileName f) (filePath f) []
  runDB $ insert f'
  return f'

getServers :: MonadIO m => MagicT m [FileServer]
getServers = do
  res :: [Entity M.FileServer] <- runDB $ selectList [M.FileServerActive ==. True] []
  return $ Prelude.map entityVal res

