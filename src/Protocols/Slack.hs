module Protocols.Slack (respond) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)

import Chat.Message (Message, message)
import Hasklet.All (unleashUpon)

import Happstack.Server
  ( Response
  , RqData
  , ServerPart
  , badRequest
  , body
  , getDataFn
  , look
  , ok
  , toResponse
  )

tokenEnvVar :: String
tokenEnvVar = "SLACK_TOKEN"

--
-- public functions
--

respond :: ServerPart Response
respond = do
  v <- hasValidToken
  case v of True  -> genReply
            False -> bad "unauthorized request"

--
-- private functions
--

good, bad :: String -> ServerPart Response
good = ok . toResponse
bad  = badRequest . toResponse

formatMsg :: String -> String -> Message
formatMsg from text = message from' text
  where from' = '@' : from

genReply :: ServerPart Response
genReply = do
  r <- getDataFn msgFromPost
  case r of (Left e)    -> bad $ unlines e
            (Right msg) -> good $ unleashUpon msg

hasValidToken :: ServerPart Bool
hasValidToken = do
  tActual <- liftIO $ getEnv tokenEnvVar
  tReceived <- look "token"
  return $ tActual == tReceived

msgFromPost :: RqData Message
msgFromPost = formatMsg <$> bl "user_name" <*> bl "text"
  where bl = body . look
