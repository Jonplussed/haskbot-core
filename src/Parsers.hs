module Parsers where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim

type Plugin   = Parser String
type Command  = String

-- public functions

commandWithArgs :: Command -> ([String] -> String) -> Plugin
commandWithArgs com fn = withArgs com >>= return . fn . words

commandWithText :: Command -> (String -> String) -> Plugin
commandWithText com fn = withArgs com >>= return . fn

-- private functions

withArgs :: String -> Plugin
withArgs com = do
    try $ string com >> space
    manyTill anyChar eof
