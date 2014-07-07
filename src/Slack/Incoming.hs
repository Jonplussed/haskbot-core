{-# LANGUAGE OverloadedStrings #-}

module Slack.Incoming where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

import Data.Aeson

import qualified Persistence.Redis as R
import Slack.Channel

data Incoming = Incoming { channel :: Channel
                         , text    :: T.Text
                         } deriving (Eq, Show)

instance ToJSON Incoming where
  toJSON inc = object [ "channel" .= toText (channel inc)
                      , "text"    .= text inc
                      ]

queueKeyAt :: POSIXTime -> R.Key
queueKeyAt diffTime = R.Key $ BS.pack keyStr
  where
    keyStr = "incoming-" ++ show sortID
    sortID = truncate $ diffTime * 1000000

queueValFor :: Incoming -> R.Value
queueValFor inc = R.Value . BL.toStrict $ encode inc

enqueue :: Incoming -> IO R.RedisTry
enqueue inc = do
  now <- getPOSIXTime
  let key = queueKeyAt now
      val = queueValFor inc
  return $ R.set val key
