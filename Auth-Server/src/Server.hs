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
import Api.Auth

startApp :: Int -> Config -> IO ()
startApp port cfg = run port (authApp cfg)

authApp :: Config -> Application
authApp cfg = serve api (magicToServer cfg)

magicToServer :: Config -> Server AuthAPI
magicToServer cfg = enter (convertMagic cfg >>> NT Handler) authServer

convertMagic cfg = runReaderTNat cfg <<< NT runTheMagic

--server :: CookieSettings -> JWTSettings -> Server (API auths)
--server cs jwts = protected :<|> unprotected cs jwts

authServer :: MonadIO m => ServerT AuthAPI (MagicT m)
authServer = Controller.protected 
        :<|> Controller.unprotected 

api :: Proxy AuthAPI
api = Proxy

