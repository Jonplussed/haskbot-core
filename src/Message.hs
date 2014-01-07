module Message
  ( Message (user,text)
  ) where

import Control.Applicative ((<$>), (<*>))

import Happstack.Server
  ( FromData
  , body
  , fromData
  , look
  )

data Message = Message { user :: String, text :: String }
              deriving (Eq, Show)

instance FromData Message where
  fromData = Message <$> bl "user_name" <*> bl "text"
           where bl = body . look
