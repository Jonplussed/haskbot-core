module Protocols.Slack.Handler (respond) where

-- Haskell platform libraries

import           Control.Monad.IO.Class    (liftIO)
import           System.Environment        (getEnv)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim
import           Text.Parsec.String

-- foreign libraries

import           Happstack.Server

-- native libraries

import qualified Protocols.Slack.Request   as Req
import qualified Protocols.Slack.Response  as Res
import           Registry                  (plugins)
import           Settings

-- public functions

respond :: ServerPart Response
respond = do
    req <- getDataFn Req.fromPost
    case req of
      Left errors -> badRequest . toResponse $ unlines errors
      Right m     -> validateToken m

-- private functions

validateToken :: Req.Request -> ServerPart Response
validateToken req = do
    token <- liftIO $ getEnv slackTokenEnvVar
    if token == Req.secretToken req
      then craftResponse req
      else unauthorized $ toResponse "invalid secret token"

craftResponse :: Req.Request -> ServerPart Response
craftResponse req =
    case applyPlugins req of
      Right str -> ok . toResponse $ Res.Response (Req.userName req) str
      Left err  -> badRequest . toResponse $ show err

applyPlugins :: Req.Request -> Either ParseError String
applyPlugins req = parse parser str str
  where
    parser = do
        optional $ char '@'
        string botName
        optional $ char ':'
        spaces
        choice . plugins $ Req.userName req
    str = Req.text req
