{-# LANGUAGE RecordWildCards #-}

module App.Environment
( Haskbot
, ActionH
, ScottyH
, Environment (..)
, appEnv
, appTime
) where

import Control.Monad.Reader (ReaderT)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Database.Redis as R
import qualified Network.HTTP.Conduit as N
import Web.Scotty.Trans (ActionT, ScottyT)

import App.Config (getNetworkInfo, getRedisInfo)

type Haskbot = ReaderT Environment IO
type ScottyH = ScottyT TL.Text Haskbot
type ActionH = ActionT TL.Text Haskbot

data Environment = Environment { redisConn   :: !R.Connection
                               , networkConn :: N.Manager
                               }

-- public functions

appEnv :: IO Environment
appEnv = do
  redisConn   <- getRedisInfo >>= R.connect
  networkConn <- getNetworkInfo >>= N.newManager
  return Environment {..}

appTime :: IO T.Text
appTime = getPOSIXTime >>= return . T.pack . show . truncate . (* 1000000)

-- private functions