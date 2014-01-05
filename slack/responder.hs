module Slack.Responder
  ( SlackMsg(..)
  , respondToMsg
  ) where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import System.Environment (getEnv)

import Happstack.Server
  ( FromData (fromData)
  , Response
  , RqData
  , ServerPart
  , badRequest
  , body
  , checkRq
  , getData
  , look
  , ok
  , toResponse
  , withData
  )

tokenEnvVar = "SLACK_TOKEN"

data SlackMsg =
  SlackMsg { user :: String, text :: String }
  deriving (Eq, Show)

instance FromData SlackMsg where
  fromData = SlackMsg <$> bl "user_name" <*> bl "text"
    where bl = body . look

--
-- public functions
--

respondToMsg :: ServerPart Response
respondToMsg = do
  r <- getData
  case (r) of
    (Left e)    -> badRequest . toResponse $ unlines e
    (Right msg) -> craftResponse msg

--
-- private functions
--

craftResponse :: SlackMsg -> ServerPart Response
craftResponse msg = ok . toResponse $ (user msg) ++ ": " ++ (text msg)

-- validateToken msg = look "token" `checkRq` validate
--   where validate token = do
--             t <- liftIO $ getEnv tokenEnvVar
--             if token == t
--               then return $ Right msg
--               else return $ Left "unauthorized request"
