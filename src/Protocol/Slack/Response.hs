{-# LANGUAGE OverloadedStrings #-}

module Protocol.Slack.Response
( response
) where

import Control.Monad.IO.Class (liftIO)
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Prim

import Data.Aeson hiding (json)
import Web.Scotty

import Parser.Combinator (botName)
import Parser.Plugin (pluginsFor)
import qualified Protocol.Slack.Request as R
import Type.User (getUser)

data Response = Response { userName :: String
                         , text     :: String
                         } deriving (Eq, Show)

instance ToJSON Response where
  -- We leave off the optional username in the Slack response parameters
  -- because all it does is replace Haskbot's name, not send a DM. This is
  -- subject to future change, though, as the API is currently in beta.
  toJSON (Response u t) = object [ "text" .= t ]

response :: R.Request -> ActionM ()
response req =
    case applyPlugins req of
      Right ioStr -> liftIO ioStr >>= json . Response (R.userName req)
      Left err    -> fail "cannot parse"

applyPlugins :: R.Request -> Either ParseError (IO String)
applyPlugins req = parse parser str str
  where
    parser = do
        botName
        spaces
        pluginsFor $ getUser req
    str = R.text req
