{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Directory where
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Api.Directory
import Servant.Client

directoryServer :: (String,Int) 
directoryServer = ("localhost",8080)

listFiles :: IO String
listFiles = do
        Directory.query listFiles' directoryServer 

--query :: ClientM a -> (String,Int) -> IO String
query q (host,port) = do
        manager' <- newManager defaultManagerSettings
        res <- runClientM q (ClientEnv manager' (BaseUrl Http host port ""))
        case res of
          Left err  -> return "error"
          Right res -> return $ show res
