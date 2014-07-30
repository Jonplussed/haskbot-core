{-# LANGUAGE RecordWildCards #-}

module Network.Haskbot.Internal.Environment
( Haskbot
, Environment (..)
, getAppEnv
, getSlackEndpoint
) where

import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Monad.Error (ErrorT)
import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Network.Connection as N
import qualified Network.HTTP.Conduit as N
import qualified Network.Wai as W
import System.Environment (getEnv)

type Haskbot = ReaderT Environment (ErrorT W.Response IO)

data Environment = Environment { networkConn :: N.Manager
                               , incQueue    :: TVar [BL.ByteString]
                               }

-- constants

tokenVar :: String
tokenVar = "HASKBOT_ENDPOINT"

-- public functions

getSlackEndpoint :: IO String
getSlackEndpoint = getEnv tokenVar

getAppEnv :: IO Environment
getAppEnv = do
  networkConn <- getNetworkInfo >>= N.newManager
  incQueue    <- newTVarIO []
  return $ Environment {..}

-- private functions

getNetworkInfo :: IO N.ManagerSettings
getNetworkInfo = return $ N.mkManagerSettings tlsInfo Nothing
  where tlsInfo = N.TLSSettingsSimple False False False
