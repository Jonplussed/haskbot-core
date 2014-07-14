{-# LANGUAGE OverloadedStrings #-}

module App.WebServer (webServer) where

import Control.Concurrent (forkIO)
import Control.Monad.Reader (liftIO, runReaderT)
import Data.Text.Lazy (Text, fromStrict)

import Network.HTTP.Types.Status (badRequest400, unauthorized401)
import qualified Web.Scotty.Trans as S

import App.Environment (ActionH, ScottyH, getEnv, getTimestamp)
import Registry (registry)
import Slack.Incoming (sendIncoming)
import Slack.Plugin (apply, isAuthorized, selectFrom)
import Slack.SlashCom (SlashCom, command, fromParams)

-- public functions

webServer :: Int -> IO ()
webServer port = do
    env <- getEnv
    let haskbot r = runReaderT r env
    forkIO $ haskbot sendIncoming
    S.scottyT port haskbot haskbot routes

-- private functions

applyPlugin :: SlashCom -> ActionH ()
applyPlugin slashCom =
  case selectFrom registry (command slashCom) of
    Just plugin ->
      if isAuthorized plugin slashCom
      then apply plugin slashCom    >> S.text "200 OK"
      else S.status unauthorized401 >> S.text "401 Unauthorized"
    _ -> S.status badRequest400     >> S.text "400 Bad Request"

routes :: ScottyH ()
routes = do
    S.post "/slack" $ fromParams >>= applyPlugin
    S.get  "/ping"  $ timestamp

timestamp :: ActionH ()
timestamp = liftIO getTimestamp >>= S.text . fromStrict
