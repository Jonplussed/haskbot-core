module Main
( main
) where

import Control.Monad.Reader
import System.Environment (getArgs)

import Application.TaskRunner (taskRunner)
import Application.WebServer (webServer)
import Connection.MemStore (connection)

data Env = Env

-- constants

usage :: String
usage = unlines
  [ "a haskell-based chatbot"
  , ""
  , "USAGE:"
  , "haskbot serve PORT - run as webserver listening on the specified PORT"
  , "haskbot task NAME  - run a one-off task of the given NAME"
  ]

-- public functions

main :: IO ()
main = runReaderT (liftIO $ getArgs >>= runApp) env

-- private functions

env = Env

runApp :: [String] -> IO ()
runApp ["serve",port] = webServer $ read port
runApp ("task":tasks) = taskRunner tasks
runApp _              = error usage
