module Main where

import Directory
import Locking
import Config

import Control.Monad.Reader
import Control.Monad.Except

main :: IO ()
main = do
 cfg <- getConfig
 case cfg of
  Left err -> print err
  Right cfg' -> do
    putStrLn "******* Welcome to the Distributed-File-System **********"
    loop
    return ()
    where 
      loop = do
        e <- runExceptT (runReaderT console cfg')
        case e of
          Left err -> print err
          Right _ -> print "OK!"
        loop
console :: AppT ()
console = loop
 where 
   loop = do
     cmd <- liftIO getLine
     case words cmd of
       ["ls"]            -> Directory.listFiles 
       ["read",path]     -> Directory.readFile path
       ["new",name,path] -> Directory.newFile name path
       ["write",path]    -> Directory.writeFile path
       ["delete",path]   -> Directory.deleteFile path
       ["lock",path]     -> Locking.lockFile path
       ["unlock",path]   -> Locking.unlockFile path
       _                 -> liftIO $ putStrLn $ "Help Meee!" 
                                            ++ "\nCOMMANDS:\n" 
                                            ++ "---> ls \n" 
                                            ++ "---> new <file name> <file path>\n"
                                            ++ "---> write <file name> <file path>\n"
                                            ++ "---> delete <path>\n"
                                            ++ "---> lock <path>\n"
                                            ++ "---> unlock <path>\n"
     loop 
