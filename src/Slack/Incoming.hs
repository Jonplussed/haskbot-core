{-# LANGUAGE OverloadedStrings #-}

module Slack.Incoming
( Incoming (..)
, addToSendQueue
, sendFromQueue
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Reader (ask, liftIO)
import qualified Data.ByteString as BS
import Data.Text (Text)

import Data.Aeson (ToJSON, Object, (.=), encode, object, toJSON)
import Network.HTTP.Conduit
import Network.HTTP.Types (Header, methodPost, status200)
import Web.Scotty (ActionM)

import App.Environment (Haskbot, networkConn)
import App.MemStore (Key, Value, dequeue, enqueue, fromValue, toValue, toKey)
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

incRequest :: Haskbot Request
incRequest = do
    initRequest <- parseUrl "https://bendyworks.slack.com"
    return $ initRequest
      { path              = "/services/hooks/incoming-webhook"
      , queryString       = BS.append "?token=" slackIncomingToken
      , method            = methodPost
      , rawBody           = True
      , requestHeaders    = [jsonContentType]
      }

jsonContentType :: Header
jsonContentType = ("Content-Type", "application/json")

queueKey :: Key
queueKey = toKey ("incoming-queue" :: Text)

timeBetweenSends :: Int
timeBetweenSends = 1000000 -- Slack rate limit

-- public functions

addToSendQueue :: Incoming -> Haskbot ()
addToSendQueue inc = enqueue value queueKey
  where value = toValue . encode $ toJSON inc

sendFromQueue :: Haskbot ()
sendFromQueue = forever $ dequeue queueKey >>= sendNextMsg >> wait

-- private functions

respHandler :: Value -> Response a -> Haskbot ()
respHandler json resp
  | responseStatus resp == status200 = return ()
  | otherwise = enqueue json queueKey -- should also log failure

sendNextMsg :: Maybe Value -> Haskbot ()
sendNextMsg (Just json) = do
    env <- ask
    template <- incRequest
    let newRequest = template { requestBody = RequestBodyBS $ fromValue json }
    httpLbs newRequest (networkConn env) >>= respHandler json
sendNextMsg _ = return ()

wait :: Haskbot ()
wait = liftIO $ threadDelay timeBetweenSends
