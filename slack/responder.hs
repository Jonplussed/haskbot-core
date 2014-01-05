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

tokenEnvVar :: String
tokenEnvVar = "SLACK_TOKEN"

--
-- public functions
--

respondToMsg :: ServerPart Response
respondToMsg = do
  r <- getData >>= validateToken
  case r of
    (Left e) -> badRequest . toResponse $ unlines e
    (Right msg) -> S.responseFor msg

--
-- private functions
--

validateToken :: Either [String] S.SlackMsg -> ServerPart (Either [String] S.SlackMsg)
validateToken msg = do
  tActual <- liftIO $ getEnv tokenEnvVar
  tReceived <- look "token"
  let validate m =
        if tActual == tReceived
        then Right m
        else Left ["unauthorized request"]
  return $ msg >>= validate
