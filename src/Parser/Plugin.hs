module Parser.Plugin (pluginsFor) where

import Text.Parsec.String     (Parser)
import Text.Parsec.Combinator (choice)

import Registry               (registry)
import Type.Plugin            (runPlugin)
import Type.User              (User)

pluginsFor :: User -> Parser (IO String)
pluginsFor = choice . map runPlugin . registry
