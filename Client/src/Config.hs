{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Config where

import Control.Monad.Reader
import Control.Monad.Except
import Api.Config
import Api.File 

import Data.Cache
import Data.Time

type FileCache = Cache String CachedFile
type CachedFile = (File,UTCTime)

data Config = Config {
  userId :: Int,
  directoryNode :: Node,
  lockingNode :: Node,
  cache :: FileCache
  }

data ServerErr = Error String
  deriving Show
type AppT = ReaderT Config (ExceptT ServerErr IO)



getConfig :: IO (Either String Config) 
getConfig = do
  c <- newCache Nothing :: IO FileCache
  res <- initClientServer
  case res of
    Right cfg -> return $ Right Config {
                          userId = 1,
                          directoryNode = cDirServer cfg,
                          lockingNode = cLockServer cfg,
                          cache = c
                        }
    Left _ -> return $ Left  "Error: could not connect to configuration server: Aborting."

