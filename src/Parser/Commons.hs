module Parser.Commons
( Plugin (..)
, Name
, HelpText
, InputParser
, newPlugin
, commandWithArgs
, commandWithText
) where

import Data.Text (Text)

import Text.Parsec.String (Parser)
import Parser.Combinators (withArgs)

type Name        = String
type Command     = String
type HelpText    = Text
type InputParser = Parser String

data Plugin = Plugin { plName     :: Name
                     , plHelpText :: HelpText
                     , plParser   :: InputParser
                     }

-- public functions

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

commandWithArgs
  -- the first word of the message text, identifying the command
  :: Command
  -- a function taking the rest of the message words as a list of arguments
  -- and returning a new string
  -> ([String] -> String)
  -- creates your parser for you!
  -> InputParser
commandWithArgs com fn = withArgs com >>= return . fn . words

commandWithText
  -- the first word of the message text, identifying the command
  :: Command
  -- a function taking the rest of the message as a string of text
  -- and returning a new string
  -> (String -> String)
  -- creates your parser for you!
  -> InputParser
commandWithText com fn = withArgs com >>= return . fn
