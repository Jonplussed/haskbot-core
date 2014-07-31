{-# LANGUAGE OverloadedStrings #-}

-- | This provides a simple representation of the request data for a Slack
--   /incoming/ integration- the means via which Haskbot replies to Slack.
--   Currently only simple text replies are supported, but this will be expanded
--   to support fully-slack-formatted messages in the future.
module Network.Haskbot.Incoming
( Incoming (..)
, addToSendQueue
, sendFromQueue
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar', readTVar)
import Control.Monad (forever)
import Control.Monad.Error (runErrorT)
import Control.Monad.Reader (ask, liftIO)
import Data.Aeson (ToJSON, (.=), encode, object, toJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Network.Haskbot.Internal.Environment
  (HaskbotM, getSlackEndpoint, incQueue, networkConn)
import Network.Haskbot.Internal.Request (jsonContentType)
import Network.Haskbot.Types (Channel, getAddress)
import Network.HTTP.Conduit -- basically everything
import Network.HTTP.Types (Header, methodPost, status200)

data Incoming =
  Incoming { incChan :: !Channel
           -- ^ the channel to send the reply
           , incText :: {-# UNPACK #-} !Text
           -- ^ the text of the reply
           } deriving (Eq, Show)

instance ToJSON Incoming where
  toJSON inc = object [ "channel" .= getAddress (incChan inc)
                      , "text"    .= incText inc
                      ]

-- constants

timeBetweenSends :: Int
timeBetweenSends = 1000000 -- Slack rate limit

-- internal functions

addToSendQueue :: Incoming -> HaskbotM ()
addToSendQueue inc = enqueueMsg . encode $ toJSON inc

sendFromQueue :: HaskbotM ()
sendFromQueue = forever $ dequeueMsg >>= sendMsg >> wait

-- private functions

incRequest :: HaskbotM Request
incRequest = do
    endpoint    <- liftIO getSlackEndpoint
    initRequest <- liftIO $ parseUrl endpoint
    return $ initRequest
      { method            = methodPost
      , rawBody           = True
      , requestHeaders    = [jsonContentType]
      }

handleResp :: BL.ByteString -> Response a -> HaskbotM ()
handleResp msg resp
  | responseStatus resp == status200 = return ()
  | otherwise = enqueueMsg msg -- should also log failure

sendMsg :: Maybe BL.ByteString -> HaskbotM ()
sendMsg (Just msg) = do
    env <- ask
    template <- incRequest
    let newRequest = template { requestBody = RequestBodyLBS msg }
    liftIO (httpLbs newRequest $ networkConn env) >>= handleResp msg
sendMsg _ = return ()

wait :: HaskbotM ()
wait = liftIO $ threadDelay timeBetweenSends

enqueueMsg :: BL.ByteString -> HaskbotM ()
enqueueMsg msg = do
    env <- ask
    liftIO . atomically $ modifyTVar' (incQueue env) (\q -> q ++ [msg])

dequeueMsg :: HaskbotM (Maybe BL.ByteString)
dequeueMsg = do
    env <- ask
    liftIO . atomically $ do
        msgs <- readTVar $ incQueue env
        case msgs of
          (m:ms) -> do
            modifyTVar' (incQueue env) (\q -> tail q)
            return $ Just m
          _ -> return Nothing
