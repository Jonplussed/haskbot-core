module Protocols.Slack.Response
  ( Response (..)
  ) where

-- Haskell platform libraries

import qualified Data.ByteString.Char8     as B
import qualified Data.ByteString.Lazy.UTF8 as LU
import           Text.Printf ( printf )

-- foreign libraries

import Happstack.Server ( ToMessage (..) )

data Response = Response { userName :: String
                         , text     :: String
                         } deriving (Eq, Show)

instance ToMessage Response where
  toContentType _ = B.pack "application/json"
  toMessage     r = LU.fromString $ json (userName r) (text r)

json :: String -> String -> String
json = printf "{\"username\":\"%s\",\"text\":\"%s\"}"
