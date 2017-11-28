{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Controller(
putFile
)where

import Models 
import Config
import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)

putFile :: MonadIO m => File -> MagicT m FileInfo
putFile _ = return $ FileInfo "magicFOo" "remmy boyzzz" [1]


