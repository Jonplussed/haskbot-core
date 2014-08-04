{-# LANGUAGE OverloadedStrings #-}

module Network.Haskbot.Internal.Server
( webServer
) where

import Control.Concurrent (forkIO)
import Control.Monad.Error (runErrorT, throwError)
import Control.Monad.Reader (runReaderT)
import Network.Haskbot.Config (Config, listenOn)
import Network.Haskbot.Internal.Monad (HaskbotM)
import Network.Haskbot.Internal.Request (getPostParams, headOnly, paramsMap)
import Network.Haskbot.Incoming (sendFromQueue)
import Network.Haskbot.Plugin (Plugin, isAuthorized, runPlugin, selectFrom)
import Network.Haskbot.SlashCommand (SlashCom, command, fromParams)
import Network.HTTP.Types (ok200, badRequest400, unauthorized401)
import Network.Wai (Request, Response)
import Network.Wai.Handler.Warp (run)

-- internal functions

webServer :: Config -> [Plugin] -> IO ()
webServer config plugins = do
    forkIO $ sendResponsesToSlack config
    processSlackRequests config plugins

-- private functions

sendResponsesToSlack :: Config -> IO ()
sendResponsesToSlack = runReaderT sendFromQueue

processSlackRequests :: Config -> [Plugin] -> IO ()
processSlackRequests config plugins =
  run (listenOn config) (\req resp -> runner config plugins req >>= resp)

runner :: Config -> [Plugin] -> Request -> IO Response
runner config plugins req = do
  ranOrFailed <- runErrorT $ runReaderT (pipeline plugins req) config
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
