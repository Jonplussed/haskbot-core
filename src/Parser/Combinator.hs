module Parser.Combinator
( botName
, text
, args
, optArgs
) where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

-- public functions

botName :: Parser String
botName = string "haskbot"

text :: Parser String
text = space >> many1Till anyChar eof

args :: Parser [String]
args = text >>= return . words

optArgs :: Parser [String]
optArgs = optText >>= return . words

-- private functions

optText :: Parser String
optText = option "" $ try text

many1Till :: (Show end, Stream s m t)
          => ParsecT s u m a
          -> ParsecT s u m end
          -> ParsecT s u m [a]
many1Till p end = do
    notFollowedBy end
    first <- p
    rest  <- manyTill p end
    return $ first : rest
