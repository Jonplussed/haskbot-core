{-# LANGUAGE OverloadedStrings #-}

module Network.Haskbot.Internal.Request where

import Control.Monad.Error (throwError)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as M
import qualified Network.HTTP.Types as N
import qualified Network.Wai as W
import Network.Haskbot.Internal.Environment

type Params = M.Map Text Text

-- constants

jsonContentType :: N.Header
jsonContentType = (N.hContentType, "application/json")

textContentType :: N.Header
textContentType = (N.hContentType, "text/plain")

-- internal functions

getPostParams :: W.Request -> HaskbotM Params
getPostParams req
    | isPost    = return $ paramsMap req
    | otherwise = throwError N.badRequest400
  where
    isPost = W.requestMethod req == N.methodPost

headOnly :: N.Status -> W.Response
headOnly status = W.responseLBS status [] . fromStrict $ N.statusMessage status

paramsMap :: W.Request -> Params
paramsMap req = M.fromList $ map decode bsParams
  where
    bsParams = N.parseSimpleQuery $ W.rawQueryString req
    decode (k,v) = (decodeUtf8 k, decodeUtf8 v)

optParam :: Params -> Text -> HaskbotM (Maybe Text)
optParam pMap key = return $ M.lookup key pMap

reqParam :: Params -> Text -> HaskbotM Text
reqParam pMap key =
  case M.lookup key pMap of
    Just p -> return p
    _      -> throwError N.badRequest400
