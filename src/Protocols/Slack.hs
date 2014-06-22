module Protocols.Slack
  ( respond
  ) where

-- Haskell platform libraries

import           Control.Applicative
  ( (<$>)
  , (<*>)
  )
import           Control.Monad.IO.Class
  ( liftIO
  )
import qualified Data.ByteString.Char8     as B
import qualified Data.ByteString.Lazy.UTF8 as LU
import           System.Environment
 ( getEnv
 )
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim
import           Text.Parsec.String
import           Text.Printf

-- Happstack

import Happstack.Server

-- native libraries

import Hasklets (hasklets)
import Settings

data SlackMsg = SlackMsg { secretToken :: String
                         , channelName :: String
                         , timeStamp   :: String
                         , msgUserName    :: String
                         , msgText        :: String
                         } deriving (Eq, Show)

data SlackResp = SlackResp { respUserName :: String
                           , respText     :: String
                           } deriving (Eq, Show)

instance ToMessage SlackResp where
  toContentType _ = B.pack "application/json"
  toMessage     r = LU.fromString $ printf json (respUserName r) (respText r)
    where json = "{\"username\":\"%s\",\"text\":\"%s\"}"

-- public functions

respond :: ServerPart Response
respond = parseRequest

-- private functions

parseRequest :: ServerPart Response
parseRequest = do
    msg <- getDataFn slackMsg
    case msg of
      Left errors -> badRequest . toResponse $ unlines errors
      Right m     -> validateToken m

validateToken :: SlackMsg -> ServerPart Response
validateToken msg = do
    token <- liftIO $ getEnv slackTokenEnvVar
    if token == secretToken msg
      then craftResponse msg
      else unauthorized $ toResponse "invalid secret token"

craftResponse :: SlackMsg -> ServerPart Response
craftResponse msg =
    case applyHasklets msg of
      Right str -> ok . toResponse $ SlackResp (msgUserName msg) str
      Left err -> badRequest . toResponse $ show err

slackMsg :: RqData SlackMsg
slackMsg = SlackMsg <$> bl "token"
                    <*> bl "channel_name"
                    <*> bl "timestamp"
                    <*> bl "user_name"
                    <*> bl "text"
  where bl = body . look

applyHasklets :: SlackMsg -> Either ParseError String
applyHasklets msg = parse parser str str
  where
    parser = do
        optional $ char '@'
        string chatbotName
        optional $ char ':'
        spaces
        choice hasklets
    str = msgText msg
