module Chat.Message
  ( Message
  , from
  , message
  , text
  ) where

data Message = Message { from :: String, text :: String }
             deriving (Eq, Show)

--
-- public functions
--

message :: String -> String -> Message
message f t = Message f t
