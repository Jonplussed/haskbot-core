module Main (main) where

import qualified Connectors.Web as Web

main :: IO ()
main = Web.server
