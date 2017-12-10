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
  res <- initLockServer "localhost" (read port)
  case res of
    Right _ -> do 
      putStrLn $ "Locking server running on port: "++ port
      startApp (read port) c
    Left _ -> putStrLn "Error connecting to configuration server: Aborting."
 
