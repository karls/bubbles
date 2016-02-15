{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Monoid ((<>))
import Network.EngineIO.Wai
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
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

port :: Int
port = 3001

corsPolicy = const (Just (simpleCorsResourcePolicy { corsOrigins = Just (["http://localhost:8000", "http://siphon.local:8000"], True)}))

main :: IO ()
main = do
    sHandler <- SocketIO.initialize waiAPI eioServer
    putStrLn $ "Running on " <> show port
    run port $ cors corsPolicy $ app sHandler
