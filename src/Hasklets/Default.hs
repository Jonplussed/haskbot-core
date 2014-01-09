module Hasklets.Default (passiveSigh) where

import Hasklet.Skeleton (Hasklet, hasklet)

passiveSigh :: Hasklet
passiveSigh = hasklet trigger reaction

trigger :: String
trigger = "."

reaction :: String -> [String] -> String
reaction username _ = "/sighs passive-aggressively at " ++ username
