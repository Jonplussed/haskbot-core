{-# LANGUAGE RecordWildCards #-}

module Network.Haskbot.Internal.Environment
( HaskbotM
, Environment (..)
, getAppEnv
, getSlackEndpoint
) where

import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Monad.Error (Error, ErrorT)
import Control.Monad.Error.Class (noMsg, strMsg)
import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Network.Connection as N
import qualified Network.HTTP.Conduit as N
import Network.HTTP.Types (Status, internalServerError500, mkStatus)

import System.Environment (getEnv)

data Environment = Environment { networkConn :: N.Manager
                               , incQueue    :: TVar [BL.ByteString]
                               }

type HaskbotM = ReaderT Environment (ErrorT Status IO)

instance Error Status where
  noMsg  = internalServerError500
  strMsg = mkStatus 500 . B8.pack

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
