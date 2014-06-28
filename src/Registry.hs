module Registry (registry) where

import           Type.Plugin  (Plugin, plParser)
import           Type.User    (User)
import qualified Plugins.Flip as Flip

-- this breaks the circular dependency required by the "help" plugin
import {-# SOURCE #-} qualified Plugins.Help as Help

registry :: User -> [Plugin]
registry user =

  [ Flip.plugin
  , Help.pluginFor user
  ]
