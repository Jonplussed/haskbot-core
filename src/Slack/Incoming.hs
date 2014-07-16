{-# LANGUAGE OverloadedStrings #-}

module Slack.Incoming
( Incoming (..)
, addToSendQueue
, sendIncoming
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Reader (liftIO)
import Data.Text (Text)

import Data.Aeson (ToJSON, Object, (.=), encode, object, toJSON)
import Web.Scotty (ActionM)

import App.Environment (Haskbot)
import App.MemStore (Key, Value, dequeue, enqueue, fromValue, toValue, toKey)
import App.Network
import Config (slackIncomingToken)
import Slack.Types (Channel, getAddress)

data Incoming = Incoming { incChan ::                !Channel
                         , incText :: {-# UNPACK #-} !Text
                         } deriving (Eq, Show)

instance ToJSON Incoming where
  toJSON inc = object [ "channel" .= getAddress (incChan inc)
                      , "text"    .= incText inc
                      ]

-- constants

queueKey :: Key
queueKey = toKey ("incoming-queue" :: Text)

timeBetweenSends :: Int
timeBetweenSends = 1000000

sendURL :: String
sendURL = concat
  [ "https://bendyworks.slack.com/services/hooks/incoming-webhook"
  , "?token="
  , slackIncomingToken
  ]

-- public functions

addToSendQueue :: Incoming -> Haskbot ()
addToSendQueue inc = enqueue value queueKey
  where value = toValue . encode $ toJSON inc

returnToSendQueue :: Value -> Haskbot ()
returnToSendQueue json = enqueue json queueKey

-- this shit be cray; plz simplify
sendIncoming :: Haskbot ()
sendIncoming =
  forever $ do
      outbound <- dequeue queueKey
      case outbound of
        Just json -> do
            succ <- liftIO . sendAsJSON sendURL $ fromValue json
            case succ of
                Failure _ -> returnToSendQueue json
                _         -> return ()
        _         -> return ()
      liftIO $ threadDelay timeBetweenSends
