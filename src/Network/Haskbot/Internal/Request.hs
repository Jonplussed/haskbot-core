{-# LANGUAGE OverloadedStrings #-}

module Network.Haskbot.Internal.Request where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M
import Network.HTTP.Types (Header, hContentType, parseSimpleQuery)
import qualified Network.Wai as W
import Network.Haskbot.Internal.Environment

type Params = M.Map T.Text T.Text

-- constants

jsonContentType :: Header
jsonContentType = (hContentType, "application/json")

textContentType :: Header
textContentType = (hContentType, "text/plain")

-- internal functions

paramsMap :: W.Request -> Params
paramsMap req = M.fromList $ map decode bsParams
  where
    bsParams = parseSimpleQuery $ W.rawQueryString req
    decode (k,v) = (T.decodeUtf8 k, T.decodeUtf8 v)

optParam :: Params -> T.Text -> HaskbotM (Maybe T.Text)
optParam pMap key = return $ M.lookup key pMap

reqParam :: Params -> T.Text -> HaskbotM T.Text
reqParam pMap key =
  case M.lookup key pMap of
    Just p -> return p
    _      -> fail $ concat ["param \"", T.unpack key, "\" not found!"]
