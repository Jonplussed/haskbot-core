module App.Environment
( Haskbot
, Environment (..)
) where

import Control.Monad.Reader (ReaderT)

import Database.Redis (Connection)

type Haskbot = ReaderT Environment IO

data Environment = Environment { memStoreConn :: !Connection }
