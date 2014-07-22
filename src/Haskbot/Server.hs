{-# LANGUAGE OverloadedStrings #-}

module Haskbot.Server (webServer) where

import Control.Concurrent (forkIO)
import Control.Monad.Reader (lift, liftIO, runReaderT)
import Data.Text.Lazy (fromStrict)

import Network.HTTP.Types.Status (badRequest400, unauthorized401)
import Web.Scotty.Trans (get, post, scottyT, status, text)

import Haskbot.Environment (ActionH, ScottyH, getAppEnv, getAppTime)
import Haskbot.Slacklet (Slacklet, isAuthorized, runSlacklet, selectFrom)
import Slack.Incoming (sendFromQueue)
import Slack.SlashCom (SlashCom, command, fromParams)

-- public functions

webServer :: [Slacklet] -> Int -> IO ()
webServer slacklets port = do
    env <- getAppEnv
    let haskbot r = runReaderT r env
    forkIO $ haskbot sendFromQueue
    scottyT port haskbot haskbot $ routes slacklets

-- private functions

findAndRun :: [Slacklet] -> SlashCom -> ActionH ()
findAndRun slacklets slashCom =
  case selectFrom slacklets (command slashCom) of
    Just p ->
      if isAuthorized p slashCom
      then lift (runSlacklet p slashCom) >> text "200 OK"
      else status unauthorized401      >> text "401 Unauthorized"
    _ -> status badRequest400          >> text "400 Bad Request"

routes :: [Slacklet] -> ScottyH ()
routes slacklets = do
    post "/slack" $ fromParams >>= findAndRun slacklets
    get  "/ping"  $ timestamp

timestamp :: ActionH ()
timestamp = liftIO getAppTime >>= text . fromStrict
