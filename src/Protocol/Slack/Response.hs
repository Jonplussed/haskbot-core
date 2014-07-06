{-# LANGUAGE OverloadedStrings #-}

module Protocol.Slack.Response
( response
) where

import Control.Monad.IO.Class (liftIO)

import Data.Aeson hiding (json)
import Web.Scotty

import Parser.Plugin (applyPlugins)
import Type.SlackMsg (SlackMsg)

data Response = Response { text :: String
                         } deriving (Eq, Show)

instance ToJSON Response where
  toJSON (Response t) = object [ "text" .= t ]

response :: SlackMsg -> ActionM ()
response slackMsg =
  case applyPlugins slackMsg of
    Right ioStr -> liftIO ioStr >>= json . Response
    Left err    -> fail "cannot parse"
