module Parser.Combinator
( atBotName
, withArgs
, withOptionalArgs
) where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

-- constants

botName :: String
botName = "haskbot"

-- public functions

atBotName :: Parser ()
atBotName = do
    optional $ char '@'
    string botName
    optional $ char ':'

withArgs :: String -> Parser String
withArgs com = do
    try $ string com
    option "" .  try $ space >> manyTill anyChar eof

withOptionalArgs :: String -> Parser String
withOptionalArgs com = do
    try $ string com
    space
    manyTill anyChar eof
