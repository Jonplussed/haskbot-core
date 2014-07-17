{-# LANGUAGE OverloadedStrings #-}

module App.WebServer (webServer) where

import Control.Concurrent (forkIO)
import Control.Monad.Reader (lift, liftIO, runReaderT)
import Data.Text.Lazy (fromStrict)

import Network.HTTP.Types.Status (badRequest400, unauthorized401)
import Web.Scotty.Trans (get, post, scottyT, status, text)

import App.Environment (ActionH, ScottyH, appEnv, appTime)
import Registry (registry)
import Slack.Incoming (sendFromQueue)
import Slack.Plugin (apply, isAuthorized, selectFrom)
import Slack.SlashCom (SlashCom, command, fromParams)

-- public functions

webServer :: Int -> IO ()
webServer port = do
    env <- appEnv
    let haskbot r = runReaderT r env
    forkIO $ haskbot sendFromQueue
    scottyT port haskbot haskbot routes

-- private functions

applyPlugin :: SlashCom -> ActionH ()
applyPlugin slashCom =
  case selectFrom registry (command slashCom) of
    Just plugin ->
      if isAuthorized plugin slashCom
      then lift (apply plugin slashCom) >> text "200 OK"
      else status unauthorized401       >> text "401 Unauthorized"
    _ -> status badRequest400           >> text "400 Bad Request"

routes :: ScottyH ()
routes = do
    post "/slack" $ fromParams >>= applyPlugin
    get  "/ping"  $ timestamp

timestamp :: ActionH ()
timestamp = liftIO appTime >>= text . fromStrict
