module Main where

import Database.Persist.Sql (runSqlPool)

import DirectoryApi
import Config
import Models (doMigrations)

main :: IO ()
main = do
  c <- getConfig
  runSqlPool doMigrations (pool c) 
  startApp c
