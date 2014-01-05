module Main (main) where

import Control.Monad (msum)
import qualified Slack.Responder as S

import Happstack.Server
  ( BodyPolicy
  , Response
  , ServerPart
  , decodeBody
  , defaultBodyPolicy
  , dir
  , nullConf
  , simpleHTTP
  )

--
-- public functions
--

main :: IO ()
main = simpleHTTP nullConf $ do
  decodeBody bodyPolicy
  msum routes

--
-- private functions
--

bodyPolicy :: BodyPolicy
bodyPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

routes :: [ServerPart Response]
routes = [ dir "slack" S.respondToMsg ]
