module Main where

import Database.Persist.Sql (runSqlPool)

import Server
import Config
import Models (doMigrations)

main :: IO ()
main = do
  c <- getConfig
  runSqlPool doMigrations (pool c) 
  startApp c
