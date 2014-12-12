{-# LANGUAGE OverloadedStrings #-}

module Network.Haskbot.Internal.Server where

import Control.Concurrent (forkIO)
import Control.Monad.Error (runErrorT, throwError)
import Control.Monad.Reader (runReaderT)
import Haskbot.Config (Config, listenOn)
import Haskbot.Internal.Environment (Environment, HaskbotM, bootstrap, config)
import Haskbot.Internal.Request (getPostParams, headOnly)
import Haskbot.Incoming (sendFromQueue)
import Haskbot.Plugin (Plugin, isAuthorized, runPlugin, selectFrom)
import Haskbot.SlashCommand (SlashCom, command, fromParams)
import Network.HTTP.Types (ok200, badRequest400, unauthorized401)
import Network.Wai (Request, Response, Application)
import Network.Wai.Handler.Warp (run)

-- internal functions

webServer :: Config -> [Plugin] -> IO ()
webServer config plugins = do
    env <- bootstrap config
    forkIO $ sendResponsesToSlack env
    processSlackRequests env plugins

-- private functions

sendResponsesToSlack :: Environment -> IO ()
sendResponsesToSlack = runReaderT sendFromQueue

processSlackRequests :: Environment -> [Plugin] -> IO ()
processSlackRequests env plugins = run port (app env plugins)
  where
    port = listenOn $ config env

app :: Environment -> [Plugin] -> Application
app env plugins req resp = runner env plugins req >>= resp

runner :: Environment -> [Plugin] -> Request -> IO Response
runner env plugins req = do
  ranOrFailed <- runErrorT $ runReaderT (pipeline plugins req) env
  case ranOrFailed of
    Right _          -> return $ headOnly ok200
    Left errorStatus -> return $ headOnly errorStatus

pipeline :: [Plugin] -> Request -> HaskbotM ()
pipeline plugins req = getPostParams req >>= fromParams >>= findAndRun plugins

findAndRun :: [Plugin] -> SlashCom -> HaskbotM ()
findAndRun plugins slashCom =
  case selectFrom plugins (command slashCom) of
    Just plugin ->
      if isAuthorized plugin slashCom
      then runPlugin plugin slashCom
      else throwError unauthorized401
    _ -> throwError badRequest400
