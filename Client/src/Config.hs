{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Config where

import Control.Monad.Reader
import Control.Monad.Except
import Api.Config

--type Node = (String,Int)

data Config = Config {
  userId :: Int,
  directoryNode :: Node,
  lockingNode :: Node
  }

type App = ReaderT Config IO 

getConfig :: IO (Either String Config) 
getConfig = do
  res <- initClientServer
  case res of
    Right cfg -> do return $ Right $ Config {
                          userId = 1,
                          directoryNode = cDirServer cfg,
                          lockingNode = cLockServer cfg
                        }
    Left _ -> return $ Left  "Error: could not connect to configuration server: Aborting."

