module Main where

import Server
import Client
import System.Environment

main :: IO ()
main = do
  cmd <- getArgs
  case cmd of
    ["server"] -> do
      putStrLn "Server is running.."
      runServer
    ["client"] -> do
      putStrLn "Starting client..."
      runClient
