module Main where

import Database.Persist.Sql (runSqlPool)
import System.Environment   (getArgs)

import Server 
import Config
import Database (doMigrations)
import Api.Config

main :: IO ()
main = do
  [port] <- getArgs
  c <- getConfig
  runSqlPool doMigrations (pool c) 
  res <- initDirServer "localhost" (read port)
  case res of
    Left _  -> putStrLn "Error connecting to Config server: Aborting."
    Right _ -> do 
      putStrLn $ "Directory server running on port: "++ port
      startApp (read port) c
