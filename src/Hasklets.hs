module Hasklets (hasklets) where

import Text.Parsec.String
import Hasklets.Flipper (angryFlip)

type Hasklet = Parser String

hasklets :: [Hasklet]
hasklets = [ angryFlip
           ]
