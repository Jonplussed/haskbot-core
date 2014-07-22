{-# LANGUAGE OverloadedStrings #-}

module Slack.Incoming
( Incoming (..)
, addToSendQueue
, sendFromQueue
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar', readTVar)
import Control.Monad (forever)
import Control.Monad.Reader (ask, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)

import Data.Aeson (ToJSON, (.=), encode, object, toJSON)
import Network.HTTP.Conduit -- basically everything
import Network.HTTP.Types (Header, methodPost, status200)

import Haskbot.Environment (Haskbot, getSlackToken, incQueue, networkConn)
import Slack.Types (Channel, getAddress)

data Incoming = Incoming { incChan ::                !Channel
                         , incText :: {-# UNPACK #-} !Text
                         } deriving (Eq, Show)

instance ToJSON Incoming where
  toJSON inc = object [ "channel" .= getAddress (incChan inc)
                      , "text"    .= incText inc
                      ]

-- constants

jsonContentType :: Header
jsonContentType = ("Content-Type", "application/json")

slackUrl :: String
slackUrl = "https://bendyworks.slack.com/services/hooks/incoming-webhook"

timeBetweenSends :: Int
timeBetweenSends = 1000000 -- Slack rate limit

-- public functions

addToSendQueue :: Incoming -> Haskbot ()
addToSendQueue inc = enqueueMsg . encode $ toJSON inc

sendFromQueue :: Haskbot ()
sendFromQueue = forever $ dequeueMsg >>= sendMsg >> wait

-- private functions

incRequest :: Haskbot Request
incRequest = do
    token       <- liftIO getSlackToken
    initRequest <- parseUrl slackUrl
    return $ initRequest
      { queryString       = BS.append "?token=" $ BS.pack token
      , method            = methodPost
      , rawBody           = True
      , requestHeaders    = [jsonContentType]
      }

handleResp :: BL.ByteString -> Response a -> Haskbot ()
handleResp msg resp
  | responseStatus resp == status200 = return ()
  | otherwise = enqueueMsg msg -- should also log failure

sendMsg :: Maybe BL.ByteString -> Haskbot ()
sendMsg (Just msg) = do
    env <- ask
    template <- incRequest
    let newRequest = template { requestBody = RequestBodyLBS msg }
    httpLbs newRequest (networkConn env) >>= handleResp msg
sendMsg _ = return ()

wait :: Haskbot ()
wait = liftIO $ threadDelay timeBetweenSends

enqueueMsg :: BL.ByteString -> Haskbot ()
enqueueMsg msg = do
    env <- ask
    liftIO . atomically $ modifyTVar' (incQueue env) (\q -> q ++ [msg])

dequeueMsg :: Haskbot (Maybe BL.ByteString)
dequeueMsg = do
    env <- ask
    liftIO . atomically $ do
        msgs <- readTVar $ incQueue env
        case msgs of
          (m:ms) -> do
            modifyTVar' (incQueue env) (\q -> tail q)
            return $ Just m
          _ -> return Nothing
