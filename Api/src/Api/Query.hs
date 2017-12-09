{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Query where

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client


send q (host,port) = do
        manager' <- newManager defaultManagerSettings
        runClientM q (ClientEnv manager' (BaseUrl Http host port ""))
