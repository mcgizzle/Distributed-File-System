-- @Employment.hs
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric              #-}
module Api.Models where

import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Database.Persist.TH

data NodeType = FileServer | DirectoryServer | LockingServer
  deriving (Generic, Show, Read, Eq)
instance ToJSON NodeType
instance FromJSON NodeType
derivePersistField "NodeType"

