module Hasklet.Register (hasklets) where

import Hasklet.Skeleton (Hasklet)
import Hasklets.Flipper (angryFlip)

hasklets :: [Hasklet]
hasklets =
  [ angryFlip
  ]
