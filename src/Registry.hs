{-# LANGUAGE OverloadedStrings #-}

module Registry (registry) where

import Slack.Plugin (Plugin)
import Slack.SlashCom (SlashCom)
import qualified Plugin.Flip as Flip
import qualified Plugin.Status as Status

-- this breaks the circular dependency required by the "help" plugin
import {-# SOURCE #-} qualified Plugin.Help as Help

registry :: [Plugin]
registry =

  [ Flip.register   "flip token"
  , Status.register "status token"
  , Help.register   "help token"
  ]
