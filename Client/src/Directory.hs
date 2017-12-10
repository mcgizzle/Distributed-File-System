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


directoryServer :: (String,Int) 
directoryServer = ("localhost",8080)

getAbsPath :: FileInfo -> String
getAbsPath f = fileInfoFile_path f ++ fileInfoFile_name f

readFile :: FilePath -> App ()
readFile path = do
  res <- findFile path
  case res of
       Right file -> liftIO $ putStrLn $ "File Contents:\n" ++ fileContents file
       Left err   -> liftIO $ print err

listFiles :: App ()
listFiles = do
        res <- sendDirectory listFiles'
        case res of
          Right res' -> liftIO $ do 
                  putStrLn "FILES:"
                  mapM_ (\ f -> do 
                          putStrLn $ "Name: " ++ fileInfoFile_name f
                          putStrLn $ "Path: " ++ getAbsPath f ++ "\n") res'
          Left err   -> liftIO $ putStrLn $ "There was an error: " ++ show err

newFile :: String -> String -> App ()
newFile name path = do
        contents <- liftIO $ runUserEditorDWIM plainTemplate "HERE IS YOUR NEW TEXT FILE\n"
        let file = File name path ( wrapStr contents)
        time <- liftIO $ getCurrentTime
        res <- sendDirectory (newFile' file)
        case res of
          Right res' -> do
            cacheFile file time
            liftIO $ putStrLn "File Saved!\n"
          Left err   -> liftIO $ putStrLn $ "There was an error: " ++ show err

writeFile :: FilePath -> App ()
writeFile path = do
  locked <- checkLock path
  when locked $ do 
    liftIO $ putStrLn $ "The file is locked. You cannot access it. " 
                     ++ "Please join the queue by using the 'lock' command."
    return ()
  res <- findFile path
  case res of
       Right f -> do
         contents <- liftIO $ runUserEditorDWIM plainTemplate (pack $ fileContents f)
         let file' = File (takeFileName path) (takeDirectory path) (wrapStr contents)
         time <- liftIO getCurrentTime
         res' <- sendDirectory (writeFile' file')
         case res' of 
              Right _  -> do
                cacheFile f time
                liftIO $ putStrLn "Success writing to file!\n"
              Left err -> liftIO $ putStrLn $ "Error: " ++ show err
       Left err -> liftIO $ putStrLn $ "Error: " ++ show err

findFile :: String -> App (Either ServantError File)
findFile path = do
  cached <- checkCache path
  case cached of
    Just file -> return $ Right file
    Nothing -> do 
      res <- sendDirectory (getFileLoc' (Just path))
      case res of
        Right res' -> do
          let node = (fileNodeHost $ head res', fileNodePort $ head res')
          liftIO $ Api.Query.query (getFile' (Just path)) node 
        Left err -> return $ Left err

cacheFile :: File -> UTCTime -> App ()
cacheFile f@File{..} time = do
  c <- asks cache
  liftIO $ Cache.insert c (filePath ++ fileName) (f,time)

checkCache :: FilePath -> App (Maybe File)
checkCache path = do
  c <- asks cache
  res <- liftIO $ Cache.lookup c path
  case res of
    Just (file,time) -> do
      res' <- sendDirectory (checkCache' (Just path) (Just time))
      case res' of
        Right _ -> return $ Just file
        Left _  -> return Nothing
    Nothing -> return Nothing

sendDirectory :: ClientM a -> App (Either ServantError a)
sendDirectory q = do
  dirNode <- asks directoryNode
  liftIO $ Api.Query.query q dirNode
