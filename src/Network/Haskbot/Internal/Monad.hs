module Network.Haskbot.Internal.Monad
( HaskbotM
, ConfigT
) where

import Control.Monad.Error (Error, ErrorT)
import Control.Monad.Error.Class (noMsg, strMsg)
import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString.Char8 as B8
import Network.Haskbot.Config (Config)
import Network.HTTP.Types (Status, internalServerError500, mkStatus)

type ConfigT m = ReaderT Config m
type HaskbotM = ConfigT (ErrorT Status IO)

instance Error Status where
  noMsg  = internalServerError500
  strMsg = mkStatus 500 . B8.pack
