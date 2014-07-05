module Persistence.Redis
( Key
, Value
, toKey
, fromKey
, toValue
, fromValue
, get
, getWithDefault
, set
, setWithDefault
) where

import Control.Applicative
import qualified Data.ByteString.Char8 as B

import qualified Database.Redis as R

import Type.User

newtype Key   = Key { fromKey' :: B.ByteString }
newtype Value = Value { fromValue' :: B.ByteString }

-- constants

connError :: (Monad m) => m a
connError = fail "connection to Redis server failed"

-- public functions

toKey :: String -> Key
toKey = Key . B.pack

fromKey :: Key -> String
fromKey = B.unpack . fromKey'

toValue :: String -> Value
toValue = Value . B.pack

fromValue :: Value -> String
fromValue = B.unpack . fromValue'

get :: Key -> IO (Maybe Value)
get key =
  redisConn $ do
    val <- R.get (fromKey' key)
    case val of
      Left _    -> connError
      Right v   -> return $ fmap Value v

getWithDefault :: String -> Key -> IO Value
getWithDefault def key = do
    val <- get key
    return $ case val of
      Just v -> v
      _      -> toValue def

set :: Value -> Key -> IO (Either R.Reply R.Status)
set value key = redisConn $ R.set (fromKey' key) (fromValue' value)

setWithDefault :: String -> Value -> Key -> IO Value
setWithDefault def val key = do
    status <- set val key
    case status of
      Left _  -> connError
      Right _ -> return $ toValue def

-- private functions

redisConn :: R.Redis a -> IO a
redisConn r = R.connect R.defaultConnectInfo >>= flip R.runRedis r
