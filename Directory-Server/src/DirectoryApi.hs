{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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

type DirectoryAPI = "ls"                            :> Get '[JSON] [FileInfo]
               :<|> "new"   :> ReqBody '[JSON] File :> Post '[JSON] FileInfo 
               :<|> "write" :> ReqBody '[JSON] File :> Post '[JSON] FileInfo
               :<|> QueryParam "path" FilePath      :> 
                    QueryParam "name" String        :> Get '[JSON] FileInfo  

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


