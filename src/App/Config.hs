{-# LANGUAGE OverloadedStrings #-}

module App.Config
( getNetworkInfo
, getRedisInfo
, getSlackToken
) where

import Data.ByteString (ByteString)
import System.Environment (getEnv)

import qualified Database.Redis as R
import qualified Network.Connection as N
import qualified Network.HTTP.Conduit as N

-- constants

envVar, tokenVar :: String
envVar   = "HASKBOT_ENV"
tokenVar = "HASKBOT_TOKEN"

-- public functions

getSlackToken :: IO String
getSlackToken = getEnv tokenVar

getRedisInfo :: IO R.ConnectInfo
getRedisInfo = do
  env <- getEnv envVar
  return $ case env of
    "test" -> R.defaultConnectInfo { R.connectDatabase = 0 }
    _      -> R.defaultConnectInfo { R.connectDatabase = 1 }

getNetworkInfo :: IO N.ManagerSettings
getNetworkInfo = return $ N.mkManagerSettings tlsInfo Nothing
  where tlsInfo = N.TLSSettingsSimple False False False
