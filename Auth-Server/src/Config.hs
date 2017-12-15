{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Config(
MagicT(..),Magic,
Config(..),Environment,
getConfig
)where

import Control.Monad.Except        (ExceptT, MonadError)
import Database.Persist
import Database.Persist.Postgresql (ConnectionPool, ConnectionString,
                                    createPostgresqlPool)
import Servant                     (ServantErr)
import System.Environment          (lookupEnv)

import Servant.API.Auth.Token
import Servant.Server
import Servant.Server.Auth.Token

-- Magic Monad Stuff -----------------------------------------------
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT,MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)

newtype MagicT m a
  = MagicT
  {
    runTheMagic :: ReaderT Config (ExceptT ServantErr m) a
  }deriving( Functor, Applicative, Monad, MonadReader Config, 
             MonadError ServantErr, MonadIO)

type Magic = MagicT IO

data Config = Config {
  environment :: Environment,
  pool        :: ConnectionPool,
  authConfig  :: String
}

data Environment = Development
                 | Production
                 | Test
                 deriving(Show,Eq,Read)
-------------------------------------------------------------

-- | Lift servant monad to server monad
liftHandler :: Handler a -> MagicT a
liftHandler = MagicT . lift . lift

-- | Special monad for authorisation actions
newtype AuthM a = AuthM { unAuthM :: PersistentBackendT IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError ServantErr, HasStorage, HasAuthConfig)
--
runAuth :: AuthM a -> ServerM a
runAuth m = do
  cfg <- asks authConfig
  pool <- asks pool
  liftHandler $ ExceptT $ runPersistentBackendT cfg pool $ unAuthM m


--- Environment Setup ---------------------------------------
getConfig :: IO Config
getConfig = do
  env <- getEnv
  p <- makePool env
  let auth = defaultAuthConfig
  flip runSqlPool pool $ runMigration S.migrateAllAuth
  runPersistentBackendT authConfig pool $ ensureAdmin 17 "root" "root" "root@localhost"
  return Config {
            environment = env,
            pool = p,
            authConfig = auth
          }

getEnv :: IO Environment
getEnv = do
  e <- lookupEnv "ENV"
  case e of
    Nothing -> return Development
    Just e' -> return $ read e'

makePool :: Environment -> IO ConnectionPool
makePool env = do
  s <- getConnString env
  let n = getPoolSize env
  case env of
    Development -> runStdoutLoggingT (createPostgresqlPool s n)
    Test        -> runNoLoggingT (createPostgresqlPool s n)
    Production  -> runStdoutLoggingT (createPostgresqlPool s n)


connStr = "host=localhost dbname=fs_dev user=root password=root port=5432"

getConnString :: Environment -> IO ConnectionString
getConnString _ = return connStr

getPoolSize :: Environment -> Int
getPoolSize Development = 1
getPoolSize Test = 1
getPoolSize Production = 1
--------------------------------------------------------------
