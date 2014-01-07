module Hasklets (unleashUpon) where

import Text.Regex

import Message (Message, text, user)
import Hasklet.Base (Hasklet, trigger, reaction)
import Hasklet.Flipper (angryFlipper)

--
-- public functions
--

unleashUpon :: Message -> String
unleashUpon msg = user msg ++ " said \"" ++ text msg ++ "\""

--
-- private functions
--

hasklets :: [Hasklet]
hasklets =
  [ angryFlipper
  ]
