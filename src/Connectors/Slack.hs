module Connectors.Slack
  ( server
  ) where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)

import Hasklet.All (unleashUpon)
import Chat.Message (Message)

import Happstack.Server
  ( BodyPolicy
  , Response
  , ServerPart
  , badRequest
  , decodeBody
  , defaultBodyPolicy
  , dir
  , getData
  , look
  , nullConf
  , ok
  , simpleHTTP
  , toResponse
  )

type PendingResponse = Either [String] Message

--
-- public functions
--

server :: IO ()
server = simpleHTTP nullConf $ do
  decodeBody bodyPolicy
  dir "slack" replyToMsg

--
-- private functions
--

bodyPolicy :: BodyPolicy
bodyPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

replyToMsg :: ServerPart Response
replyToMsg = do
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
