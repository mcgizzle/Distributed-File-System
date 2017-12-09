{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Config where

import Control.Monad.Reader
import Control.Monad.Except

type Node = (String,Int)

data Config = Config {
  userId :: Int,
  directoryNode :: Node,
  lockingNode :: Node
                 }

type App = ReaderT Config IO 

getConfig :: IO Config
getConfig = do
  return Config {
    userId = 1,
    directoryNode = ("localhost",8080),
    lockingNode = ("localhost",8000)
         }


