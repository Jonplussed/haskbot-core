module Parser.Plugin (pluginsFor) where

import Text.Parsec.String     (Parser)
import Text.Parsec.Combinator (choice)

import Registry               (registry)
import Type.Plugin            (plParser)
import Type.User              (User)

pluginsFor :: User -> Parser String
pluginsFor = choice . map plParser . registry
