{-# LANGUAGE OverloadedStrings #-}

module Haskbot.Responder where

-- | This provides a simple representation of the request data for a Slack
--   /incoming/ integration- the means via which Haskbot replies to Slack.
--   Currently only simple text replies are supported, but this will be
--   expanded to support fully-Slack-formatted messages in the future.

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVar)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, runReaderT, asks, liftIO)

import qualified Network.Connection   as N
import qualified Network.HTTP.Conduit as N
import qualified Network.HTTP.Types   as N
import qualified Data.ByteString.Lazy as LB

import Network.HTTP.Conduit

type ResponderM  = ReaderT Responder IO
type MsgQueue = TVar [LB.ByteString]

data Responder = Responder
  { resp_queue  :: MsgQueue
  , resp_sender :: LB.ByteString -> ResponderM ()
  }

-- public functions

sendRespsToSlack :: Responder -> IO ()
sendRespsToSlack = runReaderT sendFromQueue

enqueueResp :: LB.ByteString -> ResponderM ()
enqueueResp msg = do
    queue <- asks resp_queue
    liftIO . atomically $ modifyTVar' queue $ \q -> q ++ [msg]

initResponder :: MsgQueue -> String -> Responder
initResponder queue url = Responder queue $ initSender url

-- private functions

sendFromQueue :: ResponderM ()
sendFromQueue = forever $ dequeueResp >>= sendResp >> wait1Sec

dequeueResp :: ResponderM (Maybe LB.ByteString)
dequeueResp = do
    queue  <- asks resp_queue
    liftIO . atomically $ do
        msgs <- readTVar queue
        case msgs of
            (m:_) -> do
                modifyTVar' queue $ \q -> tail q
                return $ Just m
            _ -> return Nothing

sendResp :: Maybe LB.ByteString -> ResponderM ()
sendResp (Just resp) = do
    sender <- asks resp_sender
    sender resp
sendResp _ = return ()

handleSlack :: LB.ByteString -> Response a -> ResponderM ()
handleSlack resp slack
    | allGood   = return ()
    | otherwise = enqueueResp resp
  where
    allGood = responseStatus slack == N.status200

wait1Sec :: ResponderM ()
wait1Sec = liftIO $ threadDelay 1000000

requestTo :: String -> IO Request
requestTo url = do
    req <- liftIO $ parseUrl url
    return $ req
      { method         = N.methodPost
      , rawBody        = True
      , requestHeaders = [(N.hContentType, "application/json")]
      }

initNetConn :: IO N.Manager
initNetConn = N.newManager $ N.mkManagerSettings tlsInfo Nothing
  where tlsInfo = N.TLSSettingsSimple False False False

initSender :: String -> LB.ByteString -> ResponderM ()
initSender url resp = do
    conn     <- liftIO initNetConn
    template <- liftIO $ requestTo url
    let toSend = template { requestBody = RequestBodyLBS resp }
    liftIO (httpLbs toSend conn) >>= handleSlack resp
