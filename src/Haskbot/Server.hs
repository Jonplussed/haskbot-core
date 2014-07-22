{-# LANGUAGE OverloadedStrings #-}

module Haskbot.Server (webServer) where

import Control.Concurrent (forkIO)
import Control.Monad.Reader (lift, liftIO, runReaderT)
import Data.Text.Lazy (fromStrict)

import Network.HTTP.Types.Status (badRequest400, unauthorized401)
import Web.Scotty.Trans (get, post, scottyT, status, text)

import Haskbot.Environment (ActionH, ScottyH, getAppEnv, getAppTime)
import Haskbot.Plugin (Plugin, isAuthorized, runPlugin, selectFrom)
import Slack.Incoming (sendFromQueue)
import Slack.SlashCom (SlashCom, command, fromParams)

-- public functions

webServer :: [Plugin] -> Int -> IO ()
webServer plugins port = do
    env <- getAppEnv
    let haskbot r = runReaderT r env
    forkIO $ haskbot sendFromQueue
    scottyT port haskbot haskbot $ routes plugins

-- private functions

findAndRun :: [Plugin] -> SlashCom -> ActionH ()
findAndRun plugins slashCom =
  case selectFrom plugins (command slashCom) of
    Just p ->
      if isAuthorized p slashCom
      then lift (runPlugin p slashCom) >> text "200 OK"
      else status unauthorized401      >> text "401 Unauthorized"
    _ -> status badRequest400          >> text "400 Bad Request"

routes :: [Plugin] -> ScottyH ()
routes plugins = do
    post "/slack" $ fromParams >>= findAndRun plugins
    get  "/ping"  $ timestamp

timestamp :: ActionH ()
timestamp = liftIO getAppTime >>= text . fromStrict
