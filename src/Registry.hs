module Registry (pluginsFor) where

import Text.Parsec.String     (Parser)
import Text.Parsec.Combinator (choice)

import Plugins.TableFlip      (tableFlip)
import Types.User             (User)

pluginsFor :: User -> Parser String
pluginsFor user = choice $

  [ tableFlip
  ]
