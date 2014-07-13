module App.Environment
( Haskbot
, ScottyH
, ActionH
, Environment (..)
, getEnv
, getTimestamp
) where

import Control.Monad.Reader (ReaderT)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Database.Redis as R
import Web.Scotty.Trans (ScottyT, ActionT)

import Config (redisConnInfo)

type Haskbot = ReaderT Environment IO
type ScottyH = ScottyT TL.Text Haskbot
type ActionH = ActionT TL.Text Haskbot

data Environment = Environment { memStoreConn :: !R.Connection }

-- public functions

getEnv :: IO Environment
getEnv =  R.connect redisConnInfo >>= return . Environment

getTimestamp :: IO T.Text
getTimestamp = getPOSIXTime >>= return . T.pack . show . truncate . (* 1000000)
