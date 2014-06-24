module Registry (pluginsFor) where

import Text.Parsec.String     (Parser)
import Text.Parsec.Combinator (choice)
import Plugins.TableFlip      (tableFlip)

pluginsFor :: String -> Parser String
pluginsFor userName = choice $

  [ tableFlip
  ]
