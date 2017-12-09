{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-} 

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

import Control.Monad.Reader

import Config
import Locking

directoryServer :: (String,Int) 
directoryServer = ("localhost",8080)

getAbsPath :: FileInfo -> String
getAbsPath f = (fileInfoFile_path f ++ fileInfoFile_name f)

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
        res <- sendDirectory (newFile' file)
        case res of
          Right res' -> liftIO $ putStrLn "File Saved!\n"
          Left err   -> liftIO $ putStrLn $ "There was an error: " ++ show err

writeFile :: FilePath -> App ()
writeFile path = do
  locked <- checkLock path
  when locked $ do 
    liftIO $ putStrLn "The file is locked. You cannot access it."
    return ()
  res <- findFile path
  case res of
       Right f -> do
         contents <- liftIO $ runUserEditorDWIM plainTemplate (pack $ fileContents f)
         let file' = File (takeFileName path) (takeDirectory path) (wrapStr contents)
         res' <- sendDirectory (writeFile' file')
         case res' of 
              Right _  -> liftIO $ putStrLn "Success writing to file!\n"
              Left err -> liftIO $ putStrLn $ "Error: " ++ show err
       Left err -> liftIO $ putStrLn $ "Error: " ++ show err

findFile :: String -> App (Either ServantError File)
findFile path = do
  res <- sendDirectory (getFileLoc' (Just path))
  case res of
       Right res' -> do
         let node = (fileNodeHost $ head res', fileNodePort $ head res')
         liftIO $ Api.Query.send (getFile' (Just path)) node 
       Left err -> return $ Left err


sendDirectory :: ClientM a -> App (Either ServantError a)
sendDirectory q = do
  dirNode <- asks directoryNode
  liftIO $ Api.Query.send q dirNode
