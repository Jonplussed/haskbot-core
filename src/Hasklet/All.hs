module Hasklet.All (unleashUpon) where

import Text.Regex (matchRegex, mkRegex)

import Chat.Message (Message, from, text)
import Hasklet.Register (hasklets)
import Hasklet.Skeleton (Hasklet, trigger, reaction)
import Hasklets.Default (passiveSigh)

--
-- public functions
--

unleashUpon :: Message -> String
unleashUpon msg = applyHasklet msg hasklets

--
-- private functions
--

-- this explicit recursion can probably be replaced with something better
applyHasklet :: Message -> [Hasklet] -> String
applyHasklet msg [] = (reaction passiveSigh) (from msg) []
applyHasklet msg (h:hs) =
  case (matchRegex regex $ text msg) of
    Just matches -> (reaction h) (from msg) matches
    Nothing        -> applyHasklet msg hs
  where regex = mkRegex $ trigger h
