module Directory where

import Api.Directory

directoryServer :: (String,Int)
directoryServer = ("localhost",8080)

listFiles :: IO String
listFiles = do
        query listFiles' directoryServer 

query :: ClientM a -> (String,Int) -> String
query q (host,port) = do
        manager' <- newManager defaultManagerSettings
        res <- runClientM q (ClientEnv manager' (BaseUrl http host port ""))
        case res of
          Left err  -> return err
          Right res -> return res
