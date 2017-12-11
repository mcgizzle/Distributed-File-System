{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Controller(
)where

import Database.Persist
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import Config
import Database

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO, lift)

protected :: MonadIO => AuthResult User -> MagicT m Protected
protected (Authenticated user) = return (token user,UserId user)
protected _                    = throwError errUnAuthorized

unprotected :: MonadIO => MagicT m Unprotected 
unprotected = do
  (cs, jwts) <- getSettings
  checkCreds cs jwts 

getSettings :: (MonadIO, MonadReader Config) => m (CookieSettings, JWTSettings)
getSettings = do
  cs <- asks cookieSet
  jwts <- asks jwtSet
  return (cs,jwts)


