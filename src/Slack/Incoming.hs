{-# LANGUAGE OverloadedStrings #-}

module Slack.Incoming where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Data.Aeson (ToJSON, Object, (.=), encode, object, toJSON)
import Web.Scotty (ActionM)

import qualified Connection.MemStore as M
import Slack.Channel

data Incoming = Incoming { channel :: Channel
                         , text    :: T.Text
                         } deriving (Eq, Show)

instance ToJSON Incoming where
  toJSON inc = object [ "channel" .= toText (channel inc)
                      , "text"    .= text inc
                      ]

-- constants

queueKey :: M.Key
queueKey = M.toKey ("incoming-queue" :: T.Text)

-- public functions

enqueue :: Incoming -> IO ()
enqueue inc = M.enqueue value queueKey
  where value = M.toValue . encode $ toJSON inc

--dequeue :: ActionM ()
--dequeue = M.dequeue queueKey >> sendToSlack

--sendToSlack :: M.Value -> ActionM ()
--sendToSlack (Value v) =
