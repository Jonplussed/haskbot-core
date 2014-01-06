module Slack.Message
  ( SlackMsg(..)
  ) where

import Control.Applicative

import Happstack.Server
  ( FromData (fromData)
  , body
  , look
  )

data SlackMsg = SlackMsg { user :: String, text :: String }
              deriving (Eq, Show)

instance FromData SlackMsg where
  fromData = SlackMsg <$> bl "user_name" <*> bl "text"
           where bl = body . look
