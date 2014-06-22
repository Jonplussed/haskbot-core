module Connections.Web (server) where

-- foreign libraries

import Happstack.Server
  ( BodyPolicy
  , decodeBody
  , defaultBodyPolicy
  , dir
  , nullConf
  , simpleHTTP
  )

-- native libraries

import qualified Protocols.Slack.Handler as SH

-- public functions

server :: IO ()
server = simpleHTTP nullConf $ do
  decodeBody bodyPolicy
  dir "slack" SH.respond

-- private functions

bodyPolicy :: BodyPolicy
bodyPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000
