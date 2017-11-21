{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


module Api where

import GHC.Generics
import Servant.API
import Data.Aeson

data File = File {
  
  fileName :: String,
  fileContents :: String

} deriving(Generic,Show)
instance ToJSON File
instance FromJSON File


data ServerInfo = ServerInfo  {
  
  fileSaved :: Bool

} deriving(Generic,Show)
instance ToJSON ServerInfo
instance FromJSON ServerInfo

type API = Capture "path" String :> Get '[JSON] File 
      :<|> ReqBody '[JSON] File :> Post '[JSON] ServerInfo


