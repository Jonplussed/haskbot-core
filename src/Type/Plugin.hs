module Type.Plugin
( Plugin (..)
, Name
, HelpText
, InputParser
, newPlugin
) where

import Data.Text (Text)
import Text.Parsec.String (Parser)

type Name        = String
type HelpText    = Text
type InputParser = Parser String

data Plugin = Plugin { plName     :: Name
                     , plHelpText :: HelpText
                     , plParser   :: InputParser
                     }

newPlugin
  -- the arbitrary name of your plugin
  :: Name
  -- the help text displayed by the "help" command
  -> HelpText
  -- the parser that consuming the input text
  -> InputParser
  -- creates the full plugin
  -> Plugin
newPlugin = Plugin
