module Main where

import Database.Persist.Sql (runSqlPool)
import System.Environment   (getArgs)

import LockingApi
import Config
import Models (doMigrations)

main :: IO ()
main = do
  [port] <- getArgs
  c <- getConfig
  runSqlPool doMigrations (pool c) 
  putStrLn $ "Locking server running on port: "++ port
  startApp (read port) c
  
