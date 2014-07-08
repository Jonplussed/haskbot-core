{-# LANGUAGE OverloadedStrings #-}

module Slack.Channel
( Channel (..)
, toText
, fromText
) where

import qualified Data.Text as T

import Slack.Types

data Channel = DirectMsg { to   :: UserName }
             | Channel   { name :: ChannelName }
             deriving (Eq, Show)

-- constants

dmPrefix, chPrefix :: Char
dmPrefix = '@'
chPrefix = '#'

-- public functions

toText :: Channel -> T.Text
toText (DirectMsg dm) = T.append (T.singleton dmPrefix) (getUserName dm)
toText (Channel ch)   = T.append (T.singleton chPrefix) (getChanName ch)

fromText :: T.Text -> Channel
fromText t
  | T.head t == dmPrefix = DirectMsg . UserName $ T.tail t
  | T.head t == chPrefix = Channel . ChannelName $ T.tail t
  | otherwise            = Channel $ ChannelName t
