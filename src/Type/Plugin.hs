module Type.Plugin
( Plugin (plName, plHelpText)
, Name
, HelpText
, InputParser
, newPlugin
, runPlugin
) where

import Data.Text          (Text)
import Text.Parsec.String (Parser)
import Text.Parsec.Char   (string)
import Text.Parsec.Prim   (try)

type Name        = String
type HelpText    = Text
type InputParser = Parser String

data Plugin = Plugin { plName     :: Name
                     , plHelpText :: HelpText
                     , plParser   :: InputParser
                     }

newPlugin :: Name -> HelpText -> InputParser -> Plugin
newPlugin = Plugin

runPlugin :: Plugin -> Parser String
runPlugin p = do
    try . string $ plName p
    plParser p
