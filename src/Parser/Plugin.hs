module Parser.Plugin (applyPlugins) where

import qualified Data.Text as T
import Text.Parsec.Combinator (choice)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (parse)
import Text.Parsec.String (Parser)

import Registry (registry)
import Type.Plugin (runPlugin)
import Type.SlackMsg

-- public function

applyPlugins :: SlackMsg -> Either ParseError (IO String)
applyPlugins slackMsg = parse parser str str
  where
    parser = pluginsFor slackMsg
    str = T.unpack $ text slackMsg

-- private functions

pluginsFor :: SlackMsg -> Parser (IO String)
pluginsFor = choice . map runPlugin . registry
