module Persistence.Redis
( Key       (..)
, Value     (..)
, Hash      (..)
, RedisTry
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

fromKey :: Key -> Text
fromKey = decodeUtf8 . fromKey'

toValue :: Text -> Value
toValue = Value . encodeUtf8

fromValue :: Value -> Text
fromValue = decodeUtf8 . fromValue'

get :: Key -> IO (Maybe Value)
get key =
  redisConn $ do
    val <- R.get (fromKey' key)
    case val of
      Left _    -> connError
      Right v   -> return $ fmap Value v

getWithDefault :: Text -> Key -> IO Value
getWithDefault def key = do
    val <- get key
    return $ case val of
      Just v -> v
      _      -> toValue def

set :: Value -> Key -> RedisTry
set value key = redisConn $ R.set (fromKey' key) (fromValue' value)

setWithDefault :: Text -> Value -> Key -> IO Value
setWithDefault def val key = do
    status <- set val key
    case status of
      Left _  -> connError
      Right _ -> return $ toValue def

-- private functions

redisConn :: R.Redis a -> IO a
redisConn r = R.connect R.defaultConnectInfo >>= flip R.runRedis r
