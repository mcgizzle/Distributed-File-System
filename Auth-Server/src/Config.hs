module Config where

import Database.Persist
import Database.Persist.Postgresql 
import Servant                     (ServantErr)
import System.Environment          (lookupEnv)
import Control.Monad.Except

import Servant.API.Auth.Token
import Control.Monad.Base
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Monoid

import Servant.Server.Auth.Token as Auth


import Database.Persist.Sql
import Servant.Server
import Servant.Server.Auth.Token.Config
import Servant.Server.Auth.Token.Model
import Servant.Server.Auth.Token.Persistent
import qualified Servant.Server.Auth.Token.Persistent.Schema as S

-- Magic Monad Stuff -----------------------------------------------
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT,MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)

class Monad m => AuthMonad m where 
  getAuthConfig :: m AuthConfig 
  liftAuthAction :: ExceptT ServantErr IO a -> m a 



newtype MagicT a
  = MagicT
  {
    runTheMagic :: ReaderT Config (ExceptT ServantErr IO) a 
  }deriving( Functor, Applicative, Monad, MonadReader Config, 
             MonadError ServantErr, MonadIO)
instance AuthMonad MagicT where 
  getAuthConfig = asks authConfig
  liftAuthAction = MagicT. lift
     

data Config = Config {
  environment :: Environment,
  pool        :: ConnectionPool,
  authConfig  :: AuthConfig
}

data Environment = Development
                 | Production
                 | Test
                 deriving(Show,Eq,Read)
-------------------------------------------------------------

--- Environment Setup ---------------------------------------
getConfig :: IO Config
getConfig = do
  env <- getEnv
  p <- makePool env
  let auth = defaultAuthConfig
  flip runSqlPool p $ runMigration Auth.migrateAll
  runPersistentBackendT auth p $ ensureAdmin 17 "root" "root" "root@localhost"
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
