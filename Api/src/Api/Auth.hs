{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Api.Auth where

import Servant
import Data.Aeson           (FromJSON, ToJSON)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import Servant.API
import Servant.Client
import GHC.Generics         (Generic)
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    userName String
    password Text
    token Token
    UniqueUser userName
    deriving Show Eq
|]

type UserToken = (Int,Token)

data Login = Login {
    username :: String,
    password :: Text
}deriving(Generic, Eq, Show, Read)
instance ToJSON Login
instance FromJSON Login

authApi :: Proxy AuthAPI
authApi = Proxy

type Protected   = "token" :> Get '[JSON] Text
type Unprotected = "login" :> ReqBody '[JSON] Login :> PostNoContent '[JSON] 
                            ( Headers '[ Header "Set-Cookie" SetCookie,
                                         Header "Set-Cookie" SetCookie ] NoContent)

type AuthAPI = ( Auth auths User :> Protected ) 
            :<|> Unprotected 
