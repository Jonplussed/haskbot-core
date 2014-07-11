{-# LANGUAGE OverloadedStrings #-}

module Application.WebServer (webServer) where

import Control.Monad.IO.Class (liftIO)

import Network.HTTP.Types.Status (badRequest400, unauthorized401)
import qualified Web.Scotty as S

import Registry
import Slack.Plugin
import Slack.SlashCom
import Slack.Types

-- public functions

webServer :: Int -> IO ()
webServer port = S.scotty port . S.post "/slack" $ fromParams >>= applyPlugin

-- private functions

applyPlugin :: SlashCom -> S.ActionM ()
applyPlugin slashCom =
  case selectFrom registry (command slashCom) of
    Just plugin ->
      if isAuthorized plugin slashCom
      then liftIO (apply plugin slashCom) >> S.text "200 OK"
      else S.status unauthorized401       >> S.text "401 Unauthorized"
    _ -> S.status badRequest400           >> S.text "400 Bad Request"
