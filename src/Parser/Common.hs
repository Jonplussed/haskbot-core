module Parser.Common
( withArgs
, withOptArgs
, withText
) where

import Parser.Combinator (text, args, optArgs)
import Type.Plugin       (InputParser)

type Command = String

-- public functions

withArgs :: ([String] -> String) -> InputParser
withArgs fn = args >>= return . fn

withOptArgs :: ([String] -> String) -> InputParser
withOptArgs fn = optArgs >>= return . fn

withText :: (String -> String) -> InputParser
withText fn = text >>= return . fn
