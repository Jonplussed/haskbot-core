module Registry (plugins) where

-- Haskell platform libraries

import Text.Parsec.String (Parser)

-- native libraries

import Plugins.TableFlip (tableFlip)
import Parsers           (Plugin)

type UserName = String

-- public functions

plugins :: UserName -> [Plugin]
plugins withUser =

  [ tableFlip
  ]
