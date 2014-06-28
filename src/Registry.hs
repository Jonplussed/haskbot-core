module Registry (pluginsFor) where

import Text.Parsec.String          (Parser)
import Text.Parsec.Combinator      (choice)

import Parser.Commons              (Plugin, plParser)
import qualified Plugins.TableFlip as Flip
import Types.User                  (User)

registry :: User -> [Plugin]
registry user =

  [ Flip.plugin
  ]

pluginsFor :: User -> Parser String
pluginsFor = choice . map plParser . registry
