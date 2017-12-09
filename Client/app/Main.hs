module Main where

import Directory
import Config

import Control.Monad.Reader

getConfig :: IO Config
getConfig = do
  return Config {
    userId = 1,
    directoryNode = ("localhost",8080),
    lockingNode = ("localhost",8000)
         }

main :: IO ()
main = do
 cfg <- getConfig
 putStrLn "******* Welcome to the Distributed-File-System **********"
 runReaderT console cfg

console :: App ()
console = loop
 where 
   loop = do
     cmd <- liftIO getLine
     case words cmd of
       ["ls"]            -> Directory.listFiles 
       ["read",path]     -> Directory.readFile path
       ["new",name,path] -> Directory.newFile name path
       ["write",path]    -> Directory.writeFile path
       _                 -> liftIO $ putStrLn $ "Wrong command u fool, r u thick or summin?" 
                                            ++ "\nCOMMANDS:\n" 
                                            ++ "---> ls \n" 
                                            ++ "---> new <file name> <file path>\n"
                                            ++ "---> write <file name> <file path>\n"
     loop 
