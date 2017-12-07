module Main where

import Directory

main :: IO ()
main = do
        putStrLn "Welcome to the distributed file system :o"
        cmd <- getLine
        case words cmd of
          ["ls"] -> do
                  res <- Directory.listFiles
                  putStrLn $ "And the server says:\n" ++ res

