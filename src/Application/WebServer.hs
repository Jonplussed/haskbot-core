{-# LANGUAGE OverloadedStrings #-}

module Application.WebServer (webServer) where

import Control.Monad.IO.Class (liftIO)

import Network.HTTP.Types.Status (badRequest400, unauthorized401)
import Web.Scotty

import Registry (registry)
import Slack.Plugin (apply, isAuthorized, selectFrom)
import Slack.SlashCom (SlashCom, fromParams)
import Slack.Types (getCommand)

-- public functions

webServer :: Int -> IO ()
webServer port = scotty port . post "/slack" $ fromParams >>= applyPlugin

-- private functions

applyPlugin :: SlashCom -> ActionM ()
applyPlugin slashCom =
  case selectFrom registry slashCom of
    Just plugin ->
      if isAuthorized plugin slashCom
      then liftIO (apply plugin slashCom) >> text "200 OK"
      else status unauthorized401         >> text "401 Unauthorized"
    _ -> status badRequest400             >> text "400 Bad Request"
