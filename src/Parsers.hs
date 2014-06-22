module Parsers where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim

type Plugin   = Parser String
type UserName = String
type Command  = String

-- public functions

commandWithArgs :: Command -> ([String] -> String) -> Plugin
commandWithArgs com fn = do
    args <- withArgs com
    return . fn $ words args

commandWithText :: Command -> (String -> String) -> Plugin
commandWithText com fn = do
    text <- withArgs com
    return $ fn text

-- private functions

withArgs :: String -> Plugin
withArgs com = do
    string com
    space
    manyTill anyChar eof
