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
import Database
import Controller

import Api.Directory


startApp :: Int -> Config -> IO ()
startApp port cfg = run port (directoryApp cfg)

directoryApp :: Config -> Application
directoryApp cfg = serve api (magicToServer cfg)

magicToServer :: Config -> Server DirectoryAPI
magicToServer cfg = enter (convertMagic cfg >>> NT Handler) directoryServer

convertMagic cfg = runReaderTNat cfg <<< NT runTheMagic

directoryServer :: MonadIO m => ServerT DirectoryAPI (MagicT m)
directoryServer = Controller.listFiles
             :<|> Controller.newFile
             :<|> Controller.writeFile
             :<|> Controller.getFile

api :: Proxy DirectoryAPI
api = Proxy


