module Hasklet.All (unleashUpon) where

import Text.Regex (matchRegex, mkRegex)

import Chat.Message (Message, text, user)
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
applyHasklet msg [] = (reaction passiveSigh) (user msg) []
applyHasklet msg (h:hs) =
  case (matchRegex regex $ text msg) of
    Just matches -> (reaction h) (user msg) matches
    Nothing        -> applyHasklet msg hs
  where regex = mkRegex $ trigger h
