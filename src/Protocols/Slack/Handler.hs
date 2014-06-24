module Protocols.Slack.Handler (respond) where

import           Control.Monad.IO.Class   (liftIO)
import           System.Environment       (getEnv)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim
import           Text.Parsec.String

import           Happstack.Server

import           Parser.Combinators       (atBotName)
import           Protocols.Authorizable   (isAuthorized)
import qualified Protocols.Slack.Request  as Req
import qualified Protocols.Slack.Response as Res
import           Registry                 (pluginsFor)
import           Settings

-- public functions

respond :: ServerPart Response
respond = do
    tryReq <- getData
    case tryReq of
      Left errors -> badRequest . toResponse $ unlines errors
      Right req   -> authorize req

-- private functions

authorize :: Req.Request -> ServerPart Response
authorize req = if   isAuthorized req
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
        atBotName
        spaces
        pluginsFor $ Req.userName req
    str = Req.text req
