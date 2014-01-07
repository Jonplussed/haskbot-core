module Hasklet.Base
( Hasklet
, hasklet
, reaction
, trigger
) where

type Username = String
type MatchData = [String]

data Hasklet = Hasklet { trigger  :: String
                       , reaction :: Username -> MatchData -> String
                       }

hasklet :: String -> (Username -> MatchData -> String) -> Hasklet
hasklet t r = Hasklet t r
