module Server (server) where

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

import qualified Protocols.Slack.Handler as Slack

-- public functions

server :: IO ()
server = simpleHTTP nullConf $ do
  decodeBody bodyPolicy
  dir "slack" Slack.respond

-- private functions

bodyPolicy :: BodyPolicy
bodyPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000
