{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Monoid ((<>))
import Data.Maybe (maybe)
import Network.EngineIO.Wai
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import System.Environment (lookupEnv)
import Servant
import Bubbles

import qualified Network.SocketIO as SocketIO
import qualified Control.Concurrent.STM as STM

type API = "socket.io" :> Raw

api :: Proxy API
api = Proxy

server :: WaiMonad () -> Server API
server sHandler = socketIOHandler
  where
    socketIOHandler req respond = toWaiApplication sHandler req respond


app :: WaiMonad () -> Application
app sHandler = serve api $ server sHandler

defaultPort :: Int
defaultPort = 3001

corsPolicy = const (Just (simpleCorsResourcePolicy { corsOrigins = Just ([ "http://localhost:8000"
                                                                         , "http://siphon.local:8000"
                                                                         , "http://karls.github.io"], True)}))

main :: IO ()
main = do
    sHandler <- SocketIO.initialize waiAPI eioServer
    maybePort <- lookupEnv "PORT"
    let port = maybe defaultPort read maybePort
    putStrLn $ "Running on " <> show port
    run port $ cors corsPolicy $ app sHandler
