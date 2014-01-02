module Main (main) where

import Control.Monad (msum)
import Data.Char (toLower)

import Happstack.Server
  ( FromReqURI(..)
  , dir
  , nullConf
  , ok
  , path
  , seeOther
  , simpleHTTP
  , toResponse
  )

data Subject = World | Haskell

instance FromReqURI Subject where
  fromReqURI sub =
    case (map toLower sub) of
      "haskell" -> Just Haskell
      "world"   -> Just World
      _         -> Nothing

-- does this automatically coerce? don't understand
sayHello :: Subject -> String
sayHello World    = "Hello, world!"
sayHello Haskell  = "Haskell is awesome!"

main :: IO ()
main =
  simpleHTTP nullConf $ msum
    [ dir "hello"   $ path (\s -> ok $ sayHello s)
    , dir "goodbye" $ path (\s -> path (\t -> ok $ s ++ " " ++ t))
    ]
