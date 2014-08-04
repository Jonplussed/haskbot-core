{-# LANGUAGE RecordWildCards #-}

module Network.Haskbot.Internal.Environment
( Environment (..)
, bootstrap
, HaskbotM
, EnvironT
) where

import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Monad.Error (Error, ErrorT)
import Control.Monad.Error.Class (noMsg, strMsg)
import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Network.Connection as N
import Network.Haskbot.Config (Config)
import qualified Network.HTTP.Conduit as N
import Network.HTTP.Types (Status, internalServerError500, mkStatus)

data Environment = Environment { incQueue :: TVar [BL.ByteString]
                               , netConn  :: N.Manager
                               , config   :: Config
                               }

type EnvironT m = ReaderT Environment m
type HaskbotM   = EnvironT (ErrorT Status IO)

instance Error Status where
  noMsg  = internalServerError500
  strMsg = mkStatus 500 . B8.pack

-- internal functions

bootstrap :: Config -> IO Environment
bootstrap configuration = do
  incQueue    <- newTVarIO []
  netConn     <- defNetConn >>= N.newManager
  config      <- return configuration
  return $ Environment {..}

-- private functions

defNetConn :: IO N.ManagerSettings
defNetConn = return $ N.mkManagerSettings tlsInfo Nothing
  where tlsInfo = N.TLSSettingsSimple False False False
