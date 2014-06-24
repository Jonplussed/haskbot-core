module Protocols.Slack.Request
 ( Request (..)
 ) where

import           Control.Applicative    ((<$>), (<*>))
import qualified Control.Monad.IO.Class as IO
import           System.Environment     (getEnv)

import           Happstack.Server       (FromData (..), RqData, body, look)

import           Protocols.Authorizable (Authorizable (..))
import           Settings               (slackTokenEnvVar)

data Request = Unauthorized
             | Authorized { userName  :: String
                          , text      :: String
                          , timeStamp :: String
                          } deriving (Eq, Show)

instance IO.MonadIO RqData where
  liftIO = IO.liftIO

instance FromData Request where
  fromData = doTokensMatch >>= can newRequest denyRequest

instance Authorizable Request where
  isAuthorized (Authorized _ _ _) = True
  isAuthorized _                  = False

-- private functions

bl :: String -> RqData String
bl = body . look

can :: a -> a -> Bool -> a
can yesFn noFn allowed = if allowed then yesFn else noFn

denyRequest :: RqData Request
denyRequest = return Unauthorized

doTokensMatch :: RqData Bool
doTokensMatch = do
    yourToken <- bl "token"
    myToken   <- IO.liftIO $ getEnv slackTokenEnvVar
    return $ yourToken == myToken

newRequest :: RqData Request
newRequest = Authorized <$> bl "user_name"
                        <*> bl "text"
                        <*> bl "timestamp"
