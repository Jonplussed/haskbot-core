module Chat.Message
  ( Message
  , from
  , message
  , text
  ) where

import Data.List (isInfixOf)
import Data.String (words, unwords)
import Settings (chatbotName)

data Message = Message { from :: String, text :: String }
             deriving (Eq, Show)

--
-- public functions
--

message :: String -> String -> Message
message f t = Message f $ cleanDM t

--
-- private functions
--

cleanDM :: String -> String
cleanDM text
  -- replace an all-whitespace message with ellipsis
  | null w                         = "..."
  -- remove the chatbot name from the front of the message
  | chatbotName `isInfixOf` head w = unwords $ tail w
  -- otherwise leave alone
  | otherwise                      = unwords w
  where w = words text
