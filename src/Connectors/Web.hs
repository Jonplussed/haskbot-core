module Connectors.Web (server) where

import qualified Protocols.Slack as Slack

import Happstack.Server
  ( BodyPolicy
  , decodeBody
  , defaultBodyPolicy
  , dir
  , nullConf
  , simpleHTTP
  )

--
-- public functions
--

server :: IO ()
server = simpleHTTP nullConf $ do
  decodeBody bodyPolicy
  dir "slack" Slack.respond

--
-- private functions
--

bodyPolicy :: BodyPolicy
bodyPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000
