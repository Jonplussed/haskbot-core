module Hasklets (hasklets) where

-- Haskell platform libraries

import Text.Parsec.String

-- native libraries

import Hasklets.Flipper (angryFlip)

type Hasklet = Parser String

hasklets :: [Hasklet]
hasklets = [ angryFlip
           ]
