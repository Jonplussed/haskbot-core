{-# LANGUAGE OverloadedStrings #-}

module Network.Haskbot.Internal.Server
( webServer
) where

import Control.Concurrent (forkIO)
import Control.Monad.Error (runErrorT, throwError)
import Control.Monad.Reader (runReaderT)
import Network.Haskbot.Internal.Environment (HaskbotM, Environment, getAppEnv)
import Network.Haskbot.Internal.Request (getPostParams, headOnly, paramsMap)
import Network.Haskbot.Incoming (sendFromQueue)
import Network.Haskbot.Plugin (Plugin, isAuthorized, runPlugin, selectFrom)
import Network.Haskbot.SlashCommand (SlashCom, command, fromParams)
import Network.HTTP.Types (ok200, badRequest400, unauthorized401)
import Network.Wai (Request, Response)
import Network.Wai.Handler.Warp (run)

-- internal functions

webServer :: [Plugin] -> Int -> IO ()
webServer plugins port = do
    env <- getAppEnv
    forkIO $ sendResponsesToSlack env
    processSlackRequests plugins port env

-- private functions

sendResponsesToSlack :: Environment -> IO ()
sendResponsesToSlack = runReaderT sendFromQueue

processSlackRequests :: [Plugin] -> Int -> Environment -> IO ()
processSlackRequests plugins port env =
  run port $ \req resp -> runner plugins env req >>= resp

runner :: [Plugin] -> Environment -> Request -> IO Response
runner plugins env req = do
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
