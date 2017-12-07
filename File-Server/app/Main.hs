{-# LANGUAGE DeriveGeneric              #-} 
{-# LANGUAGE EmptyDataDecls             #-} 
{-# LANGUAGE FlexibleContexts           #-} 
{-# LANGUAGE FlexibleInstances          #-} 
{-# LANGUAGE GADTs                      #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 
{-# LANGUAGE QuasiQuotes                #-} 
{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TemplateHaskell            #-} 
{-# LANGUAGE TypeFamilies               #-} 

module Main where

import Server
import System.Environment
import Control.Monad (unless)

import Api.Directory

informNetwork :: Int -> (String,Int) -> IO ()
informNetwork port (dirHost, dirPort) = loop
        where loop = do 
                resp <- query (initFileNode' "localhost" port) (dirHost,dirPort) 
                unless resp $ loop
                return ()

main :: IO ()
main = do
  [port,dirHost,dirPort] <- getArgs
  putStrLn $ "Server is running on port: " ++ port
  informNetwork (read port) (dirHost, read dirPort)
  putStrLn $ "Initialised with Directory Server"
  runServer (read port)


           
