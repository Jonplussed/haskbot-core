{-# LANGUAGE OverloadedStrings #-}

module App.WebServer (webServer) where

import Control.Monad.Reader (runReaderT)
import Data.Text.Lazy (Text)

import Network.HTTP.Types.Status (badRequest400, unauthorized401)
import qualified Web.Scotty.Trans as S

import App.Environment (ActionH, ScottyH, getEnv)
import Registry (registry)
import Slack.Plugin (apply, isAuthorized, selectFrom)
import Slack.SlashCom (SlashCom, command, fromParams)

-- public functions

webServer :: Int -> IO ()
webServer port = do
    env <- getEnv
    let haskbot r = runReaderT r env
    S.scottyT port haskbot haskbot routes

-- private functions

routes :: ScottyH ()
routes = do
    S.post "/slack" $ fromParams >>= applyPlugin

applyPlugin :: SlashCom -> ActionH ()
applyPlugin slashCom =
  case selectFrom registry (command slashCom) of
    Just plugin ->
      if isAuthorized plugin slashCom
      then apply plugin slashCom    >> S.text "200 OK"
      else S.status unauthorized401 >> S.text "401 Unauthorized"
    _ -> S.status badRequest400     >> S.text "400 Bad Request"
