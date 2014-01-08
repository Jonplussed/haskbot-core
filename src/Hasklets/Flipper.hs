module Hasklets.Flipper (angryFlip) where

import Hasklet.Skeleton (Hasklet, hasklet)

angryFlip :: Hasklet
angryFlip = hasklet trigger reaction

trigger :: String
trigger = "^[Ff]ip (.*)$"

reaction :: String -> [String] -> String
reaction username matchdata = "Go away, " ++ username ++ "!"
