{-# LANGUAGE OverloadedStrings #-}

module Network.Haskbot.Internal.Request where

import Control.Monad.Error (throwError)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as M
import Network.HTTP.Types (Header, hContentType, parseSimpleQuery)
import qualified Network.Wai as W
import Network.Haskbot.Internal.Environment

type Params = M.Map Text Text

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
    decode (k,v) = (decodeUtf8 k, decodeUtf8 v)

optParam :: Params -> Text -> HaskbotM (Maybe Text)
optParam pMap key = return $ M.lookup key pMap

reqParam :: Params -> Text -> HaskbotM Text
reqParam pMap key =
  case M.lookup key pMap of
    Just p -> return p
    _      -> throwError $ concat ["param \"", unpack key, "\" not found!"]
