{-# LANGUAGE RecordWildCards #-}

module Server.Environment
( Haskbot
, ActionH
, ScottyH
, Environment (..)
, getAppEnv
, getAppTime
, getSlackToken
) where

import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (getEnv)

import qualified Network.Connection as N
import qualified Network.HTTP.Conduit as N
import Web.Scotty.Trans (ActionT, ScottyT)

import Server.Plugin (Plugin)

type Haskbot = ReaderT Environment IO
type ScottyH = ScottyT TL.Text Haskbot
type ActionH = ActionT TL.Text Haskbot

data Environment = Environment { networkConn :: N.Manager
                               , incQueue    :: TVar [BL.ByteString]
                               , plugins     :: [Plugin]
                               }

-- constants

tokenVar :: String
tokenVar = "HASKBOT_TOKEN"

-- public functions

getAppEnv :: IO Environment
getAppEnv = do
  networkConn <- getNetworkInfo >>= N.newManager
  incQueue    <- newTVarIO []
  return Environment {..}

getAppTime :: IO T.Text
getAppTime = getPOSIXTime >>= return . T.pack . show . truncate . (* 1000000)

getSlackToken :: IO String
getSlackToken = getEnv tokenVar

-- private functions

getNetworkInfo :: IO N.ManagerSettings
getNetworkInfo = return $ N.mkManagerSettings tlsInfo Nothing
  where tlsInfo = N.TLSSettingsSimple False False False
