module Chat.Message
  ( Message
  , from
  , message
  , text
  ) where

import Data.String (words, unwords)

data Message = Message { from :: String, text :: String }
             deriving (Eq, Show)

--
-- public functions
--

-- every message begins with calling Haskbot, so to simplify we eliminate
-- the first word and condense whitespace from the get-go
-- (this will have to go if any protocols violate this)
message :: String -> String -> Message
message f t = Message f t'
  where t' = tail . unwords $ words t
