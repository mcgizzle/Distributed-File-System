{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Config where

import Control.Monad.Reader
import Control.Monad.Except
import Api.Config
import Api.Query

type AppT = ReaderT FSConfig IO 

--getConfig :: Int -> Either FSConfig ServantErr
getConfig port = initFileServer "localhost" port

