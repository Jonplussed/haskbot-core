{-# LANGUAGE OverloadedStrings #-}

module Slack.Incoming
( Incoming (..)
, enqueue
) where

import Data.Text (Text)

import Data.Aeson (ToJSON, Object, (.=), encode, object, toJSON)
import Web.Scotty (ActionM)

import App.Environment (Haskbot)
import qualified App.MemStore as M
import Slack.Types

data Incoming = Incoming { incChan ::                !Channel
                         , incText :: {-# UNPACK #-} !Text
                         } deriving (Eq, Show)

instance ToJSON Incoming where
  toJSON inc = object [ "channel" .= getAddress (incChan inc)
                      , "text"    .= incText inc
                      ]

-- constants

queueKey :: M.Key
queueKey = M.toKey ("incoming-queue" :: Text)

-- public functions

enqueue :: Incoming -> Haskbot ()
enqueue inc = M.enqueue value queueKey
  where value = M.toValue . encode $ toJSON inc

--dequeue :: ActionM ()
--dequeue = M.dequeue queueKey >> sendToSlack

--sendToSlack :: M.Value -> ActionM ()
--sendToSlack (Value v) =
