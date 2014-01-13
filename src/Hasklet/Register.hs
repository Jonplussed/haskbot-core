module Hasklet.Register (hasklets) where

import Hasklet.Skeleton (Hasklet)
import Hasklets.Default (passiveSigh)
import Hasklets.Flipper (angryFlip)

hasklets :: [Hasklet]
hasklets =
  [ angryFlip
  , passiveSigh
  ]
