{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Server 
    ( startApp
    ) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Network.Wai
import Network.Wai.Handler.Warp hiding (FileInfo)
import Servant
import Control.Category     ((<<<), (>>>))

import Config
import Controller
import Api.Config

startApp :: Int -> Config -> IO ()
startApp port cfg = run port (configApp cfg)

configApp :: Config -> Application
configApp cfg = serve api (magicToServer cfg)

magicToServer :: Config -> Server ConfigAPI
magicToServer cfg = enter (convertMagic cfg >>> NT Handler) configServer

convertMagic cfg = runReaderTNat cfg <<< NT runTheMagic

configServer :: MonadIO m => ServerT ConfigAPI (MagicT m)
configServer = Controller.fileConf
          :<|> Controller.clientConf

api :: Proxy ConfigAPI
api = Proxy

