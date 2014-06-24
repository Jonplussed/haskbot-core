module Protocols.Slack.Request
 ( Request (..)
 ) where

-- Haskell platform libraries
--
import           Control.Applicative    ((<$>), (<*>))
import qualified Control.Monad.IO.Class as IO
import           System.Environment     (getEnv)

-- foreign libraries

import Happstack.Server (FromData (..), RqData, body, look)

-- native libraries

import Settings (slackTokenEnvVar)

data Request = Unauthorized
             | Authorized { userName  :: String
                          , text      :: String
                          , timeStamp :: String
                          } deriving (Eq, Show)

instance IO.MonadIO RqData where
  liftIO = IO.liftIO

instance FromData Request where
  fromData = do
    can <- isAuthorized
    if can then newRequest else denyRequest

newRequest :: RqData Request
newRequest = Authorized <$> bl "user_name"
                        <*> bl "text"
                        <*> bl "timestamp"

denyRequest :: RqData Request
denyRequest = return Unauthorized

isAuthorized :: RqData Bool
isAuthorized = do
    yourToken <- bl "token"
    myToken   <- IO.liftIO $ getEnv slackTokenEnvVar
    return $ yourToken == myToken

bl :: String -> RqData String
bl = body . look
