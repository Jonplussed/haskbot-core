{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module App.MemStore
( Key
, Keyable (..)
, Value
, Valuable (..)
, get
, set
, enqueue
, dequeue
, flushDB
) where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask, lift, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Database.Redis as R

import App.Environment (Haskbot, redisConn)

newtype Key   = Key   { fromKey'   :: BS.ByteString }
newtype Value = Value { fromValue' :: BS.ByteString }

class Keyable a where
  toKey   :: a -> Key
  fromKey :: Key -> a

instance Keyable BL.ByteString where
  toKey   = Key . BL.toStrict
  fromKey = BL.fromStrict . fromKeyWithPre

instance Keyable BS.ByteString where
  toKey   = Key
  fromKey = fromKeyWithPre

instance Keyable String where
  toKey   = Key . BS.pack
  fromKey = BS.unpack . fromKeyWithPre

instance Keyable T.Text where
  toKey   = Key . T.encodeUtf8
  fromKey = T.decodeUtf8 . fromKeyWithPre

class Valuable a where
  toValue   :: a -> Value
  fromValue :: Value -> a

instance Valuable BL.ByteString where
  toValue   = Value . BL.toStrict
  fromValue = BL.fromStrict . fromValue'

instance Valuable BS.ByteString where
  toValue             = Value
  fromValue (Value x) = x

instance Valuable String where
  toValue   = Value . BS.pack
  fromValue = BS.unpack . fromValue'

instance Valuable T.Text where
  toValue   = Value . T.encodeUtf8
  fromValue = T.decodeUtf8 . fromValue'

-- constants

keyPrefix :: BS.ByteString
keyPrefix = "haskbot-"

errorMsg :: String
errorMsg = "connection to Redis server failed"

-- public functions

get :: Key -> Haskbot (Maybe Value)
get key = redis $ R.get (fromKey key) >>= getValue

del :: Key -> Haskbot ()
del key = redis $ R.del [fromKey key] >>= doNothing

set :: Value -> Key -> Haskbot ()
set (Value v) key = redis $ R.set (fromKey key) v >>= doNothing

dequeue :: Key -> Haskbot (Maybe Value)
dequeue key = redis $ R.lpop (fromKey key) >>= getValue

enqueue :: Value -> Key -> Haskbot ()
enqueue (Value v) key = redis $ R.rpush (fromKey key) [v] >>= doNothing

flushDB :: Haskbot ()
flushDB = redis $ R.flushdb >>= doNothing

-- private functions

redis :: R.Redis a -> Haskbot a
redis comm = do
    env <- ask
    liftIO $ R.runRedis (redisConn env) comm

onSuccess :: (a -> b) -> Either R.Reply a -> R.Redis b
onSuccess f status =
  case status of
    Left _    -> fail errorMsg -- this should log a failure
    Right val -> return $ f val

doNothing :: Either R.Reply a -> R.Redis ()
doNothing = onSuccess $ \_ -> ()

getValue :: Either R.Reply (Maybe BS.ByteString) -> R.Redis (Maybe Value)
getValue  = onSuccess $ \v -> Value <$> v

fromKeyWithPre :: Key -> BS.ByteString
fromKeyWithPre = BS.append keyPrefix . fromKey'
