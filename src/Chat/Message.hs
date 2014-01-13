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
message f t = Message f $ removeDM t

--
-- private functions
--

removeDM :: String -> String
removeDM text
  | chatbotName `isInfixOf` head w = unwords $ tail w
  | otherwise                      = unwords w
  where w = words text
