module Protocols.Authorizable where

class Authorizable a where
  isAuthorized :: a -> Bool
  isAuthorized _ = True
