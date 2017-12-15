{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

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

import Servant.API
import Servant.API.Auth.Token
import Servant.Server

import Config
import Controller
import Api

startApp :: Int -> Config -> IO ()
startApp port cfg = run port (authApp cfg)

authApp :: Config -> Application
authApp cfg = serve api (magicToServer cfg)

magicToServer :: Config -> Server Api.AuthAPI
magicToServer cfg = enter (convertMagic cfg >>> NT Handler) authServer

convertMagic cfg = runReaderTNat cfg <<< NT runTheMagic

--authServer :: MonadIO m => ServerT Api.AuthAPI (MagicT m)
authServer = testEndpoint

api :: Proxy Api.AuthAPI
api = Proxy


testEndpoint :: MonadIO m => MToken' '["test-permission"] -> MagicT m ()
testEndpoint token = do
  runAuth $ guardAuthToken token
  return ()
