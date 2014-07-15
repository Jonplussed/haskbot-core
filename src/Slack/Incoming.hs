{-# LANGUAGE OverloadedStrings #-}

module Slack.Incoming
( Incoming (..)
, addToSendQueue
, sendIncoming
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Reader (liftIO)
import qualified Data.Text as T

import Data.Aeson (ToJSON, Object, (.=), encode, object, toJSON)
import Web.Scotty (ActionM)

import App.Environment (Haskbot)
import App.MemStore (Key, dequeue, enqueue, fromValue, toKey, toValue)
import App.Network (sendAsJSON)
import Config (slackIncomingToken)
import Slack.Types (Channel, getAddress)

data Incoming = Incoming { incChan ::                !Channel
                         , incText :: {-# UNPACK #-} !T.Text
                         } deriving (Eq, Show)

instance ToJSON Incoming where
  toJSON inc = object [ "channel" .= getAddress (incChan inc)
                      , "text"    .= incText inc
                      ]

-- constants

queueKey :: Key
queueKey = toKey ("incoming-queue" :: T.Text)

timeBetweenSends :: Int
timeBetweenSends = 1000000

sendURL :: T.Text
sendURL = T.concat
  [ "https://bendyworks.slack.com/services/hooks/incoming-webhook"
  , "?token="
  , slackIncomingToken
  ]

-- public functions

addToSendQueue :: Incoming -> Haskbot ()
addToSendQueue inc = enqueue value queueKey
  where value = toValue . encode $ toJSON inc

sendIncoming :: Haskbot ()
sendIncoming =
  forever $ do
      outbound <- dequeue queueKey
      case outbound of
        Just json -> liftIO . sendAsJSON sendURL $ fromValue json
        _         -> return ()
      liftIO $ threadDelay timeBetweenSends
