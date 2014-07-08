module Persistence.Redis
( Key       (..)
, Value     (..)
, Hash      (..)
, RedisTry
, toKey
, toValue
, fromValue
, get
, set
) where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Database.Redis as R

newtype Key   = Key   { fromKey'   :: ByteString }
newtype Value = Value { fromValue' :: ByteString }
newtype Hash  = Hash  { fromHash'  :: [(ByteString, ByteString)] }

type RedisTry = IO (Either R.Reply R.Status)

-- constants

connError :: (Monad m) => m a
connError = fail "connection to Redis server failed"

-- public functions

toKey :: Text -> Key
toKey = Key . encodeUtf8

toValue :: Text -> Value
toValue = Value . encodeUtf8

fromValue :: Value -> Text
fromValue = decodeUtf8 . fromValue'

get :: Key -> IO (Maybe Value)
get (Key k) = redisConn $ R.get k >>= getValue

set :: Value -> Key -> IO ()
set (Value v) (Key k) = redisConn $ R.set k v >>= doNothing

-- private functions

redisConn :: R.Redis a -> IO a
redisConn r = R.connect R.defaultConnectInfo >>= flip R.runRedis r

onSuccess :: (a -> b) -> Either R.Reply a -> R.Redis b
onSuccess f status =
  case status of
    Left _    -> connError
    Right val -> return $ f val

doNothing :: Either R.Reply a -> R.Redis ()
doNothing = onSuccess $ \_ -> ()

getValue :: Either R.Reply (Maybe ByteString) -> R.Redis (Maybe Value)
getValue  = onSuccess $ \v -> Value <$> v
