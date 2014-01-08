module Chat.Message
  ( Message
  , text
  , user
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
  fromData = message <$> bl "user_name" <*> bl "text"
           where bl = body . look

--
-- private functions
--

message :: String -> String -> Message
message u t = Message u $ cleanUp t

-- when I feel like being programmatic, I'll program something
-- prefixes :: [String]
-- prefixes =
--   [ "@haskbot: "
--   , "haskbot: "
--   , "@haskbot:"
--   , "haskbot:"
--   ]

cleanUp :: String -> String
cleanUp txt = txt
