{-# LANGUAGE OverloadedStrings #-}

-- | The configuration type required to bootstrap "Slack.Haskbot"
module Haskbot.Config
( -- * The Config type
  Config (..)
  -- ** Database configurations
, DatabaseConfig (..)
  -- ** Server configurations
, ServerConfig (..)
  -- internal use only
, slackUrl
) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Data.Word (Word16, Word32)

data Config = Config
  { conf_server :: ServerConfig
  , conf_database :: DatabaseConfig
  }

data ServerConfig = ServerConfig
  { serv_port :: Int
  -- ^ the port on which Haskbot listens
  , serv_slackEndpoint :: Text
  -- ^ the Slack endpoint of your /incoming/ integration, usually in the
  --   form of @https://[your company name].slack.com/services/hooks/incoming-webhook@
  , serv_slackToken :: Text
  -- ^ the secret token of your /incoming/ integration
  }

data DatabaseConfig = DatabaseConfig
  { db_name :: Text
  , db_user :: Text
  , db_pass :: Text
  , db_host :: ByteString
  , db_port :: Word16
  , db_keepAliveTime :: NominalDiffTime
  , db_maxConnsCount :: Word32
  }

-- internal functions

slackUrl :: ServerConfig -> Text
slackUrl conf = serv_slackEndpoint conf <> "?token=" <> serv_slackToken conf
