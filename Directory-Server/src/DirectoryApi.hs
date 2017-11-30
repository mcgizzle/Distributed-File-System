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

module DirectoryApi 
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
import Models
import Controller

type DirectoryAPI = "ls" :> Get '[JSON] [FileInfo]
               :<|> ReqBody '[JSON] File :> Get '[JSON] FileInfo 

startApp :: Config -> IO ()
startApp cfg = run 8080 (directoryApp cfg)

directoryApp :: Config -> Application
directoryApp cfg = serve api (magicToServer cfg)

magicToServer :: Config -> Server DirectoryAPI
magicToServer cfg = enter (convertMagic cfg >>> NT Handler) directoryServer

convertMagic cfg = runReaderTNat cfg <<< NT runTheMagic

directoryServer :: MonadIO m => ServerT DirectoryAPI (MagicT m)
directoryServer = Controller.listFiles
             :<|> Controller.putFile

api :: Proxy DirectoryAPI
api = Proxy


