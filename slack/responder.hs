module Slack.Responder
  ( respondToMsg
  ) where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import qualified Slack.Message as S
import qualified Slack.Responses as S

import Happstack.Server
  ( Response
  , ServerPart
  , badRequest
  , getData
  , look
  , toResponse
  )

--
-- public functions
--

respondToMsg :: ServerPart Response
respondToMsg = do
  r <- getData >>= validateToken
  case r of (Left e)    -> badRequest . toResponse $ unlines e
            (Right msg) -> S.responseFor msg

--
-- private functions
--

tokenEnvVar :: String
tokenEnvVar = "SLACK_TOKEN"

validateToken :: Either [String] S.SlackMsg -> ServerPart (Either [String] S.SlackMsg)
validateToken msg = do
  tActual <- liftIO $ getEnv tokenEnvVar
  tReceived <- look "token"
  return $ msg >>= \m -> if tActual == tReceived
                         then Right m
                         else Left ["unauthorized request"]
