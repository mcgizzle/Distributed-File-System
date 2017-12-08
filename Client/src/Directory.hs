{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Directory where
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Api.Directory
import Api.File
import Servant.Client

import Text.Editor

import Data.ByteString.Char8 (pack)
--import Control.Monad.Trans.Class
--import Control.Monad.Trans.Either

directoryServer :: (String,Int) 
directoryServer = ("localhost",8080)

getAbsPath :: FileInfo -> String
getAbsPath f = (fileInfoFile_path f ++ fileInfoFile_name f)

listFiles :: IO ()
listFiles = do
        res <- Directory.query listFiles' directoryServer
        case res of
          Right res' -> do
                  putStrLn "FILES:"
                  mapM_ (\ f -> do 
                          putStrLn $ "Name: " ++ fileInfoFile_name f
                          putStrLn $ "Path: " ++ getAbsPath f ++ "\n") res'
          Left err   -> putStrLn $ "There was an error: " ++ show err

newFile :: String -> String -> IO ()
newFile name path = do
        contents <- runUserEditorDWIM plainTemplate "HERE IS YOUR NEW TEXT FILE\n"
        let file = File name path ( wrapStr contents)
        res <- Directory.query (newFile' file) directoryServer
        case res of
          Right res' -> putStrLn "File Saved!\n"
          Left err   -> putStrLn $ "There was an error: " ++ show err

writeFile :: String -> String -> IO ()
writeFile name path = do
        res <- Directory.query (getFileLoc' (Just path) (Just name)) directoryServer
        case res of
          Right res' -> do
                  let node = (fileNodeHost $ head res', fileNodePort $ head res')
                  res'' <- Directory.query (getFile' (Just $ path ++ name)) node 
                  case res'' of
                    Right f -> do
                            contents <- runUserEditorDWIM plainTemplate (pack $ fileContents f)
                            let file' = File name path (wrapStr contents)
                            res''' <- Directory.query (writeFile' file') directoryServer
                            case res''' of 
                              Right _  -> putStrLn "Success writing to file!\n"
                              Left err -> putStrLn $ "Error: " ++ show err
                    Left err -> putStrLn $ "Error: " ++ show err
          Left err -> putStrLn $ "Error: " ++ show err

--query :: ClientM a -> (String,Int) -> IO String
query q (host,port) = do
        manager' <- newManager defaultManagerSettings
        runClientM q (ClientEnv manager' (BaseUrl Http host port ""))
