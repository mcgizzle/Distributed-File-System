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

import Servant.API.Auth.Token
import Servant.Server
import Servant.Server.Auth.Token

import Config
import Controller


startApp :: Int -> Config -> IO ()
startApp port cfg = run port (authApp cfg)

authApp :: Config -> Application
authApp cfg = serve api (magicToServer cfg)

magicToServer :: Config -> Server AuthAPI
magicToServer cfg = enter (convertMagic cfg >>> NT Handler) AuthServer

convertMagic cfg = runReaderTNat cfg <<< NT runTheMagic

authServer :: MonadIO m => ServerT AuthAPI (MagicT m)
authServer = testEndpoint

api :: Proxy AuthAPI
api = Proxy


testEndpoint :: MToken' '["test-permission"] -> MagicT ()
testEndpoint token = do
  runAuth $ guardAuthToken token
  return ()
