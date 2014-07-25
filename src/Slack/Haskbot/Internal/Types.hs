module Slack.Haskbot.Internal.Types
( prefixedBy
) where

import qualified Data.Text as T

prefixedBy :: Char -> (T.Text -> a) -> T.Text -> a
prefixedBy pre f text
  | T.head text == pre = f $ T.tail text
  | otherwise          = f text
