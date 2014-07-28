{-# LANGUAGE OverloadedStrings #-}

module Network.Haskbot.Internal.Server (webServer) where

import Control.Concurrent (forkIO)
import Control.Monad.Reader (lift, liftIO, runReaderT)
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Haskbot.Internal.Environment (ActionH, ScottyH, getAppEnv)
import Network.Haskbot.Internal.Incoming (sendFromQueue)
import Network.Haskbot.Internal.Plugin (Plugin, isAuthorized, runPlugin, selectFrom)
import Network.Haskbot.Internal.SlashCommand (SlashCom, command, fromParams)
import Network.HTTP.Types.Status (badRequest400, unauthorized401)
import Web.Scotty.Trans (get, post, scottyT, status, text)

-- public functions

webServer :: [Plugin] -> Int -> IO ()
webServer plugins port = do
    env <- getAppEnv
    let haskbot r = runReaderT r env
    forkIO $ haskbot sendFromQueue
    scottyT port haskbot haskbot $ routes plugins

-- private functions

findAndRun :: [Plugin] -> SlashCom -> ActionH ()
findAndRun plugins slashCom =
  case selectFrom plugins (command slashCom) of
    Just p ->
      if isAuthorized p slashCom
      then lift (runPlugin p slashCom) >> text "200 OK"
      else status unauthorized401      >> text "401 Unauthorized"
    _ -> status badRequest400          >> text "400 Bad Request"

routes :: [Plugin] -> ScottyH ()
routes plugins = do
    post "/slack" $ fromParams >>= findAndRun plugins
    get  "/ping"  $ timestamp

timestamp :: ActionH ()
timestamp = do
    now <- liftIO getPOSIXTime
    let stamp = show . truncate $ now * 1000000
    text $ TL.pack stamp
