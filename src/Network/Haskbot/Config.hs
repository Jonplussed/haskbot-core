{-# LANGUAGE RecordWildCards #-}

module Network.Haskbot.Config
( Config (..)
, configure
, incUrl
) where

import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import qualified Data.ByteString.Lazy as BL
import qualified Network.Connection as N
import qualified Network.HTTP.Conduit as N
import System.Environment (getEnv)

data Config = Config { incQueue    :: TVar [BL.ByteString]
                     , netConn     :: N.Manager
                     , listenOn    :: Int
                     , incEndpoint :: String
                     , incToken    :: String
                     }

-- public functions

configure :: IO Config
configure = do
  incQueue    <- newTVarIO []
  netConn     <- defNetConn >>= N.newManager
  listenOn    <- return 8000
  incEndpoint <- error "Slack incoming endpoint required"
  incToken    <- error "Slack incoming secret token required"
  return $ Config {..}

incUrl :: Config -> String
incUrl conf = incEndpoint conf ++ "?token=" ++ incToken conf

-- private functions

defNetConn :: IO N.ManagerSettings
defNetConn = return $ N.mkManagerSettings tlsInfo Nothing
  where tlsInfo = N.TLSSettingsSimple False False False
