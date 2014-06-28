module Registry (registry) where

import           Type.Plugin       (Plugin, plParser)
import           Type.User         (User)

import qualified Plugins.TableFlip as Flip

registry :: User -> [Plugin]
registry user =

  [ Flip.plugin
  ]
