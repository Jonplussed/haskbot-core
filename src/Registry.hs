module Registry (plugins) where

-- Haskell platform libraries

import Text.Parsec.String (Parser)

-- native libraries

import Plugins.Flipper    (angryFlip)

type Plugins = [Parser String]

plugins :: Plugins
plugins = [ angryFlip
          ]
