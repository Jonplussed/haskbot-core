module Persistence.Redis where

import Control.Applicative
import qualified Data.ByteString.Char8 as B

import qualified Database.Redis as R

import Type.User

newtype Key   = Key { getKey :: B.ByteString }
newtype Value = Value { getValue :: B.ByteString }

toKey :: String -> Key
toKey = Key . B.pack

toValue :: String -> Value
toValue = Value . B.pack

-- constants

connError :: (Monad m) => m a
connError = fail "connection to Redis server failed"

redisConn :: R.Redis Value -> IO Value
redisConn r = R.connect R.defaultConnectInfo >>= flip R.runRedis r

set :: (R.RedisCtx m f) => Value -> Key -> m (f R.Status)
set value key = R.set (getKey key) (getValue value)

get :: (R.RedisCtx m f, Functor f) => Key -> m (f (Maybe Value))
get key = R.get (getKey key) >>= return . fmap (liftA Value)

getWithDefault :: String -> Key -> IO Value
getWithDefault def key =
  redisConn $ do
    val <- get key
    case val of
      Left _         -> connError
      Right Nothing  -> return $ toValue def
      Right (Just x) -> return x

setWithDefault :: String -> Value -> Key -> IO Value
setWithDefault def val key =
  redisConn $ do
    status <- set val key
    case status of
      Left _  -> connError
      Right _ -> return $ toValue def
