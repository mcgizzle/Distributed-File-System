module Main where

import Directory

main :: IO ()
main = do
        putStrLn "******* Welcome to the Distributed-File-System **********"
        loop
        where 
                loop = do
                        cmd <- getLine
                        case words cmd of
                          ["ls"]                 -> Directory.listFiles
                          ["new",name,path]      -> Directory.newFile name path
                          ["write",name,path]    -> Directory.writeFile name path
                          _                      -> putStrLn $ "Wrong command u fool, r u thick or summin?" 
                                                          ++ "\nCOMMANDS:\n" 
                                                          ++ "---> ls \n" 
                                                          ++ "---> new <file name> <file path>\n"
                                                          ++ "---> write <file name> <file path>\n"
                        loop
