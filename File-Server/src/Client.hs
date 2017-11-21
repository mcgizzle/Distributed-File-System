{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


module Client (runClient) where

import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Data.Aeson

import Api

api :: Proxy API
api = Proxy

getFile :<|> putFile = client api

queries :: ClientM (File,ServerInfo)
queries = do
  g <- getFile "foo.txt" 
  p <- putFile $ File "bar.txt" "Some info"
  return (g, p)

runClient :: IO ()
runClient = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM queries (ClientEnv manager' (BaseUrl Http "localhost" 8080 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right resp -> print resp
  
