{-# LANGUAGE OverloadedStrings #-}

module Protocols.Slack.Response
( response
) where

import           Text.Parsec.Char
import           Text.Parsec.Error
import           Text.Parsec.Prim

import           Data.Aeson              hiding (json)
import           Web.Scotty

import           Parser.Combinators
import qualified Protocols.Slack.Request as R
import           Registry

data Response = Response { username :: String
                         , text     :: String
                         } deriving (Eq, Show)

instance ToJSON Response where
  toJSON (Response u t) = object [ "username" .= u
                                 , "text"     .= t ]

response :: R.Request -> ActionM ()
response req = do
    case applyPlugins req of
      Right str -> json $ Response (R.username req) str
      Left err  -> fail "cannot parse"

applyPlugins :: R.Request -> Either ParseError String
applyPlugins req = parse parser str str
  where
    parser = do
        atBotName
        spaces
        pluginsFor $ R.username req
    str = R.text req
