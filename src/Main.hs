module Main
( main
) where

import System.Environment (getArgs)

import Application.TaskRunner (taskRunner)
import Application.WebServer (webServer)

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
main = getArgs >>= runApp

-- private functions

runApp :: [String] -> IO ()
runApp ["serve",port] = webServer $ read port
runApp ("task":tasks) = taskRunner tasks
runApp _              = error usage
