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
import Web.Scotty.Trans (ActionT, ScottyT)

import Config (getRedisInfo)

type Haskbot = ReaderT Environment IO
type ScottyH = ScottyT TL.Text Haskbot
type ActionH = ActionT TL.Text Haskbot

data Environment = Environment { memStoreConn :: !R.Connection }

-- public functions

appEnv :: IO Environment
appEnv = getRedisInfo >>= R.connect >>= return . Environment

appTime :: IO T.Text
appTime = getPOSIXTime >>= return . T.pack . show . truncate . (* 1000000)

-- private functions
