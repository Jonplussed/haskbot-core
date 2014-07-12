module Main
( main
) where

import Control.Monad.Reader
import System.Environment (getArgs)

import qualified App.MemStore as M

import App.Environment (Environment (..))
import App.TaskRunner (taskRunner)
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
runAppAs ["serve",port] = bootstrap . webServer $ read port
runAppAs ("task":tasks) = bootstrap $ taskRunner tasks
runAppAs _              = error usage

bootstrap :: IO () -> IO ()
bootstrap runAs = do
    memStore <- M.connection
    let env = Environment memStore
    runReaderT (liftIO runAs) env
