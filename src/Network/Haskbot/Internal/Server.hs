{-# LANGUAGE OverloadedStrings #-}

module Network.Haskbot.Internal.Server where

import Control.Concurrent (forkIO)
import Control.Monad.Error (ErrorT, runErrorT, catchError, throwError)
import Control.Monad.Reader (ReaderT, lift, liftIO, runReaderT)
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Haskbot.Internal.Environment (HaskbotM, Environment, getAppEnv)
import Network.Haskbot.Internal.Request (Params, getPostParams, headOnly, paramsMap)
import Network.Haskbot.Incoming (sendFromQueue)
import Network.Haskbot.Plugin (Plugin, isAuthorized, runPlugin, selectFrom)
import Network.Haskbot.SlashCommand (SlashCom, command, fromParams)
import qualified Network.HTTP.Types as N
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W

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
  W.run port $ \req resp -> runner plugins env req >>= resp

runner :: [Plugin] -> Environment -> W.Request -> IO W.Response
runner plugins env req = do
  ranOrFailed <- runErrorT $ runReaderT (pipeline plugins req) env
  case ranOrFailed of
    Right _          -> return $ headOnly N.ok200
    Left errorStatus -> return $ headOnly errorStatus

pipeline :: [Plugin] -> W.Request -> HaskbotM ()
pipeline plugins req = getPostParams req >>= fromParams >>= findAndRun plugins

findAndRun :: [Plugin] -> SlashCom -> HaskbotM ()
findAndRun plugins slashCom =
  case selectFrom plugins (command slashCom) of
    Just plugin ->
      if isAuthorized plugin slashCom
      then runPlugin plugin slashCom
      else throwError N.unauthorized401
    _ -> throwError N.badRequest400
