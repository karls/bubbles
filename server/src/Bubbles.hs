{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}


module Bubbles (eioServer) where

import Prelude hiding (mapM_)
import GHC.Generics

import Control.Monad.State.Class (MonadState)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (liftA3)
import Data.Aeson ((.=),(.:))
import Data.Foldable (mapM_)

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Network.SocketIO as SocketIO


data Bubble = Bubble { t :: Int, x :: Int, y :: Int } deriving (Show, Generic)

instance Aeson.FromJSON Bubble
instance Aeson.ToJSON Bubble

eioServer :: forall (m :: * -> *). (MonadState SocketIO.RoutingTable m, MonadIO m) => m ()
eioServer = do
  SocketIO.on "new bubble" $ \b@Bubble{..} -> do
    SocketIO.emit "bubble" b
    SocketIO.broadcast "bubble" b
