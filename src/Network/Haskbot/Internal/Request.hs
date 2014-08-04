{-# LANGUAGE OverloadedStrings #-}

module Network.Haskbot.Internal.Request
( Params
, jsonContentType
, textContentType
, getPostParams
, headOnly
, getParamsMap
, optParam
, reqParam
) where

import Control.Monad.Error (liftIO, throwError)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as M
import qualified Network.HTTP.Types as N
import qualified Network.Wai as W
import Network.Haskbot.Internal.Environment (HaskbotM)

type Params = M.Map Text Text

-- constants

jsonContentType :: N.Header
jsonContentType = (N.hContentType, "application/json")

textContentType :: N.Header
textContentType = (N.hContentType, "text/plain")

-- internal functions

headOnly :: N.Status -> W.Response
headOnly status = W.responseLBS status [] . fromStrict $ N.statusMessage status

getParamsMap :: W.Request -> IO Params
getParamsMap req = do
    body <- W.requestBody req
    return . M.fromList . map decode $ N.parseSimpleQuery body
  where
    decode (k,v) = (decodeUtf8 k, decodeUtf8 v)

getPostParams :: W.Request -> HaskbotM Params
getPostParams req
    | isPost     = liftIO $ getParamsMap req
    | otherwise = throwError N.status403
  where
  isPost = W.requestMethod req == N.methodPost

optParam :: Params -> Text -> HaskbotM (Maybe Text)
optParam pMap key = return $ M.lookup key pMap

reqParam :: Params -> Text -> HaskbotM Text
reqParam pMap key =
  case M.lookup key pMap of
    Just p -> return p
    _      -> throwError N.badRequest400
