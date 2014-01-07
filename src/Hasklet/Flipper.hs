module Hasklet.Flipper (angryFlipper) where

import Hasklet.Base (Hasklet, hasklet)

angryFlipper :: Hasklet
angryFlipper = hasklet trigger reaction

trigger :: String
trigger = ""

reaction :: String -> [String] -> String
reaction username matchdata = ""
