module App.MemStore
( Key
, Keyable (..)
, Value
, Valuable (..)
, connection
, get
, set
, enqueue
, dequeue
, destroyAll
) where

import Control.Applicative ((<$>))
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Database.Redis as R

import App.Environment (Haskbot, Environment, memStoreConn)
import Config (redisConnInfo)

newtype Key   = Key   { fromKey'   :: BS.ByteString }
newtype Value = Value { fromValue' :: BS.ByteString }

class Keyable a where
  toKey   :: a -> Key
  fromKey :: Key -> a

instance Keyable T.Text where
  toKey   = Key . T.encodeUtf8
  fromKey = T.decodeUtf8 . fromKey'

instance Keyable BS.ByteString where
  toKey           = Key
  fromKey (Key x) = x

instance Keyable BL.ByteString where
  toKey   = Key . BL.toStrict
  fromKey = BL.fromStrict . fromKey'

class Valuable a where
  toValue   :: a -> Value
  fromValue :: Value -> a

instance Valuable T.Text where
  toValue   = Value . T.encodeUtf8
  fromValue = T.decodeUtf8 . fromValue'

instance Valuable BS.ByteString where
  toValue             = Value
  fromValue (Value x) = x

instance Valuable BL.ByteString where
  toValue   = Value . BL.toStrict
  fromValue = BL.fromStrict . fromValue'

-- public functions

connection :: IO R.Connection
connection = R.connect redisConnInfo

get :: Key -> Haskbot (Maybe Value)
get (Key k) = redisConn $ R.get k >>= getValue

set :: Value -> Key -> Haskbot ()
set (Value v) (Key k) = redisConn $ R.set k v >>= doNothing

enqueue :: Value -> Key -> Haskbot ()
enqueue (Value v) (Key k) = redisConn $ R.rpush k [v] >>= doNothing

dequeue :: Key -> Haskbot (Maybe Value)
dequeue (Key k) = redisConn $ R.lpop k >>= getValue

destroyAll :: Haskbot ()
destroyAll = redisConn $ R.flushdb >>= doNothing

-- private functions

redisConn :: R.Redis a -> Haskbot a
redisConn comm = do
    env <- ask
    liftIO $ R.runRedis (memStoreConn env) comm

onSuccess :: (a -> b) -> Either R.Reply a -> R.Redis b
onSuccess f status =
  case status of
    Left _    -> fail "connection to Redis server failed"
    Right val -> return $ f val

doNothing :: Either R.Reply a -> R.Redis ()
doNothing = onSuccess $ \_ -> ()

getValue :: Either R.Reply (Maybe BS.ByteString) -> R.Redis (Maybe Value)
getValue  = onSuccess $ \v -> Value <$> v
