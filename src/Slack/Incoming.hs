{-# LANGUAGE OverloadedStrings #-}

module Slack.Incoming where

import Data.Text (Text)

import Data.Aeson (ToJSON, Object, (.=), encode, object, toJSON)
import Web.Scotty (ActionM)

import qualified Connection.MemStore as M
import Slack.Types

data Incoming = Incoming { incChan :: Channel
                         , incText :: Text
                         } deriving (Eq, Show)

instance ToJSON Incoming where
  toJSON inc = object [ "channel" .= address (incChan inc)
                      , "text"    .= incText inc
                      ]

-- constants

queueKey :: M.Key
queueKey = M.toKey ("incoming-queue" :: Text)

-- public functions

enqueue :: Incoming -> IO ()
enqueue inc = M.enqueue value queueKey
  where value = M.toValue . encode $ toJSON inc

--dequeue :: ActionM ()
--dequeue = M.dequeue queueKey >> sendToSlack

--sendToSlack :: M.Value -> ActionM ()
--sendToSlack (Value v) =
