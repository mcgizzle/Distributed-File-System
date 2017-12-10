{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Query where

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

query q (host,port) = do
  manager' <- newManager defaultManagerSettings
  runClientM q (ClientEnv manager' (BaseUrl Http host port ""))

queryBool q (host,port) = do
  res <- query q (host,port)
  case res of
    Right _ -> return True
    Left _  -> return False
