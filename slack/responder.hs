module Slack.Responder
  ( Message(..)
  , respondToMsg
  ) where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import System.Environment (getEnv)

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

data Message = Message { token :: String
                       , user  :: String
                       , text  :: String
                       } deriving (Show)

tokenEnvVar = "SLACK_TOKEN"

--
-- public functions
--

respondToMsg :: ServerPart Response
respondToMsg = do
  r <- getDataFn slackRq
  t <- liftIO $ getEnv tokenEnvVar
  case (r) of
    (Left e) ->
      badResp $ unlines e
    (Right msg) ->
      if (token msg) == t
      then craftResponse msg
      else badResp "unauthorized request"
  where badResp = badRequest . toResponse

--
-- private functions
--

craftResponse :: Message -> ServerPart Response
craftResponse msg = ok . toResponse $ (user msg) ++ ": " ++ (text msg)

-- required params: token, user_name, text
slackRq :: RqData Message
slackRq = Message <$> bl "token" <*> bl "user_name" <*> bl "text"
  where bl = body . look
