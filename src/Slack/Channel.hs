{-# LANGUAGE OverloadedStrings #-}

module Slack.Channel
( Channel (..)
, toText
) where

import Data.Text (Text, append)

import Slack.Types

data Channel = DirectMsg { to   :: UserName }
             | Channel   { name :: ChannelName }
             deriving (Eq, Show)

-- constants

dmPrefix, chPrefix :: Text
dmPrefix = "@"
chPrefix = "#"

-- public functions

toText :: Channel -> Text
toText (DirectMsg dm) = append dmPrefix $ getUserName dm
toText (Channel ch)   = append chPrefix $ getChanName ch
