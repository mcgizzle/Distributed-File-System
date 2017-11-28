{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Server
    ( startApp
    ) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp hiding (FileInfo)
import Servant
import Servant.Server 
import System.Directory
import qualified Data.Aeson.Parser
import Control.Category     ((<<<), (>>>))

import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger    (runStderrLoggingT)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

import Config
import Models
import Controller

type DirectoryAPI = ReqBody '[JSON] File :> Get '[JSON] FileInfo 

startApp :: Config -> IO ()
startApp cfg = run 8080 (directoryApp cfg)

directoryApp :: Config -> Application
directoryApp cfg = serve api (magicToServer cfg)

magicToServer :: Config -> Server DirectoryAPI
magicToServer cfg = enter (convertMagic cfg >>> NT Handler) directoryServer

convertMagic cfg = runReaderTNat cfg <<< NT runTheMagic

directoryServer :: MonadIO m => ServerT DirectoryAPI (MagicT m)
directoryServer = Controller.putFile


api :: Proxy DirectoryAPI
api = Proxy


