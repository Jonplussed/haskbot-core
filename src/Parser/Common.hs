module Parser.Common
( commandWithArgs
, commandWithText
) where

import Parser.Combinator (withArgs, withOptionalArgs)
import Type.Plugin       (InputParser)

type Command = String

-- public functions

commandWithArgs
  -- the first word of the message text, identifying the command
  :: Command
  -- a function taking the rest of the message words as a list of arguments
  -- and returning a new string
  -> ([String] -> String)
  -- creates your parser for you!
  -> InputParser
commandWithArgs com fn = withArgs com >>= return . fn . words

commandWithOptionalArgs :: Command -> ([String] -> String) -> InputParser
commandWithOptionalArgs com fn = withOptionalArgs com >>= return . fn . words

commandWithText
  -- the first word of the message text, identifying the command
  :: Command
  -- a function taking the rest of the message as a string of text
  -- and returning a new string
  -> (String -> String)
  -- creates your parser for you!
  -> InputParser
commandWithText com fn = withArgs com >>= return . fn

commandWithOptionalText :: Command -> (String -> String) -> InputParser
commandWithOptionalText com fn = withOptionalArgs com >>= return . fn
