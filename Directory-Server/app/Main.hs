module Main where

import Database.Persist.Sql (runSqlPool)
import System.Environment   (getArgs)

import DirectoryApi
import Config
import Models (doMigrations)

main :: IO ()
main = do
  [port] <- getArgs
  c <- getConfig
  runSqlPool doMigrations (pool c) 
  putStrLn $ "Directory server running on port: "++ port
  startApp (read port) c
