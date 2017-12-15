{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE RecordWildCards #-}

module Directory where
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Api.Directory
import Api.File
import Api.Query
import Servant.Client

import Text.Editor
import System.FilePath
import Data.ByteString.Char8 (pack)
import Data.Maybe
import Control.Monad.Reader
import Data.Cache as Cache
import Data.Time
import Config
import Locking

import Control.Monad.Except

getAbsPath :: FileInfo -> String
getAbsPath f = fileInfoFile_path f ++ fileInfoFile_name f

readFile :: FilePath -> AppT ()
readFile path = do
  res <- findFile path
  liftIO $ putStrLn $ "File Contents:\n" ++ fileContents res 

listFiles :: AppT ()
listFiles = do
  res <- sendDirectory listFiles' "There are no files!"
  liftIO $ do 
    putStrLn "FILES:"
    mapM_ (\ f -> do 
      putStrLn $ "Name: " ++ fileInfoFile_name f
      putStrLn $ "Path: " ++ getAbsPath f ++ "\n") res

newFile :: String -> String -> AppT ()
newFile name path = do
  contents <- liftIO $ runUserEditorDWIM plainTemplate "HERE IS UR NEW TEXT FILE\n"
  let file = File name path ( wrapStr contents)
  time <- liftIO getCurrentTime
  res <- sendDirectory (newFile' file) "Error saving new files."
  cacheFile file time
  liftIO $ putStrLn "File Saved!\n"

writeFile :: FilePath -> AppT ()
writeFile path = do
  lockFile path 
  f <- findFile path
  contents <- liftIO $ runUserEditorDWIM plainTemplate (pack $ fileContents f)
  let file' = File (takeFileName path) (takeDirectory path) (wrapStr contents)
  time <- liftIO getCurrentTime
  res' <- sendDirectory (writeFile' file') "There was an error saving the file."
  unlockFile path
  time <- liftIO getCurrentTime
  cacheFile f time
  liftIO $ putStrLn "Success writing to file!\n"
        
findFile :: String -> AppT File
findFile path = do
  cached <- checkCache path
  case cached of
    Just file -> return file
    Nothing -> do 
      res <- sendDirectory (getFileLoc' (Just path)) "Error finding the files"
      let node = (fileNodeHost $ head res, fileNodePort $ head res)
      res <- liftIO $ Api.Query.query (getFile' (Just path)) node 
      case res of
        Left  _    -> throwError $ Error "Server Error: File not found."
        Right res' -> return res'


cacheFile :: File -> UTCTime -> AppT ()
cacheFile f@File{..} time = do
  c <- asks cache
  liftIO $ Cache.insert c (filePath ++ fileName) (f,time)
  
checkCache :: FilePath -> AppT (Maybe File)
checkCache path = do
  c <- asks cache
  res <- liftIO $ Cache.lookup c path
  case res of
    Just (file,time) -> do
      res' <- sendDirectory (checkCache' (Just path) (Just time)) "Error fetching file"
      if res' == InDate then return $ Just file
              else return Nothing
    Nothing -> return Nothing

sendDirectory :: ClientM a -> String -> AppT a
sendDirectory q err = do
  dirNode <- asks directoryNode
  res <- liftIO $ Api.Query.query q dirNode
  case res of
    Left  _    -> throwError $ Error err
    Right res' -> return res'

