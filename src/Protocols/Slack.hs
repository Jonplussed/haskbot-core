module Protocols.Slack (respond) where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)

import Chat.Message (Message)
import Hasklet.All (unleashUpon)

import Happstack.Server
  ( Response
  , ServerPart
  , badRequest
  , getData
  , look
  , ok
  , toResponse
  )

type PendingResponse = Either [String] Message

respond :: ServerPart Response
respond = do
  r <- getData >>= validateToken
  case r of (Left e)    -> badRequest . toResponse $ unlines e
            (Right msg) -> ok . toResponse $ unleashUpon msg

tokenEnvVar :: String
tokenEnvVar = "SLACK_TOKEN"

validateToken :: PendingResponse -> ServerPart (PendingResponse)
validateToken msg = do
  tActual <- liftIO $ getEnv tokenEnvVar
  tReceived <- look "token"
  return $ msg >>= \m -> if tActual == tReceived
                         then Right m
                         else Left ["unauthorized request"]
