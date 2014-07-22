{-# LANGUAGE OverloadedStrings #-}

module Haskbot.Server (webServer) where

import Control.Concurrent (forkIO)
import Control.Monad.Reader (lift, liftIO, runReaderT)
import Data.Text.Lazy (Text, fromStrict)

import Network.HTTP.Types.Status (badRequest400, unauthorized401)
import Web.Scotty.Trans (get, post, scottyT, status, text)

import Haskbot.Environment (ActionH, ScottyH, getAppEnv, getAppTime)
import Haskbot.Plugin (apply, isAuthorized, selectFrom)
import Slack.Incoming (sendFromQueue)
import Slack.SlashCom (SlashCom, command, fromParams)

-- public functions

--webServer :: Int -> [Plugin] -> IO ()
webServer haskbot port = do
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
timestamp = liftIO getAppTime >>= text . fromStrict
