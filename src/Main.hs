module Main
( main
) where

import System.Environment (getArgs)

import App.WebServer (webServer)

-- constants

usage :: String
usage = unlines
    [ "a Haskell-based Slack chatbot"
    , ""
    , "USAGE:"
    , "haskbot serve PORT - run as webserver listening on the specified PORT"
    , "haskbot task NAME  - run a one-off task of the given NAME"
    ]

-- public functions

main :: IO ()
main = getArgs >>= runAppAs

-- private functions

runAppAs :: [String] -> IO ()
runAppAs ["serve",port] = webServer $ read port
runAppAs _              = error usage
