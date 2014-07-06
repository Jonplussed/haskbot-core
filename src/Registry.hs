module Registry (registry) where

import Type.Plugin (Plugin)
import Type.SlackMsg (SlackMsg)
import qualified Plugins.Flip as Flip

-- this breaks the circular dependency required by the "help" plugin
import {-# SOURCE #-} qualified Plugins.Help as Help

registry :: SlackMsg -> [Plugin]
registry slackMsg = map (\f -> f slackMsg)

  [ Flip.plugin
  , Help.plugin
  ]
