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

import Config
import Controller

import Api.Locking

startApp :: Int -> Config -> IO ()
startApp port cfg = run port (lockingApp cfg)

lockingApp :: Config -> Application
lockingApp cfg = serve api (magicToServer cfg)

magicToServer :: Config -> Server LockingAPI
magicToServer cfg = enter (convertMagic cfg >>> NT Handler) lockingServer

convertMagic cfg = runReaderTNat cfg <<< NT runTheMagic

lockingServer :: MonadIO m => ServerT LockingAPI (MagicT m)
lockingServer = Controller.lockFile
           :<|> Controller.unlockFile

api :: Proxy LockingAPI
api = Proxy


