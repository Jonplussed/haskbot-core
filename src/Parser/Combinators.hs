module Parser.Combinators where

-- Haskell platform libraries

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim

-- native libraries

import Settings

atBotName :: Parser ()
atBotName = do
    optional $ char '@'
    string botName
    optional $ char ':'

withArgs :: String -> Parser String
withArgs com = do
    try $ string com >> space
    manyTill anyChar eof
