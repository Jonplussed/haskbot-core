module Registry (plugins) where

import Parser.Commons     (Plugin)
import Plugins.TableFlip (tableFlip)

plugins :: String -> [Plugin]
plugins userName =

  [ tableFlip
  ]
