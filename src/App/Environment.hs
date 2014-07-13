module App.Environment
( Haskbot
, ScottyH
, ActionH
, Environment (..)
, getEnv
) where

import Control.Monad.Reader (ReaderT)
import Data.Text.Lazy (Text)

import qualified Database.Redis as R
import Web.Scotty.Trans (ScottyT, ActionT)

import Config (redisConnInfo)

type Haskbot = ReaderT Environment IO
type ScottyH = ScottyT Text Haskbot
type ActionH = ActionT Text Haskbot

data Environment = Environment { memStoreConn :: !R.Connection }

-- public functions

getEnv :: IO Environment
getEnv = do
    memStore <- R.connect redisConnInfo
    return $ Environment memStore
