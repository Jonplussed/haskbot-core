module Parser.Commons where

import Text.Parsec.String (Parser)
import Parser.Combinators (withArgs)

type Plugin  = Parser String
type Command = String

-- public functions

commandWithArgs
  -- the first word of the message text, identifying the command
  :: Command
  -- a function taking the rest of the message words as a list of arguments
  -- and returning a new string
  -> ([String] -> String)
  -- magically creates ready-to-use plugin
  -> Plugin
commandWithArgs com fn = withArgs com >>= return . fn . words

commandWithText
  -- the first word of the message text, identifying the command
  :: Command
  -- a function taking the rest of the message as a string of text
  -- and returning a new string
  -> (String -> String)
  -- magically creates a ready-to-use plugin
  -> Plugin
commandWithText com fn = withArgs com >>= return . fn
