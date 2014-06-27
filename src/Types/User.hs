module Types.User
( UserName (..)
, UUID (..)
, User (..)
, HasUser (..)
, fromStrings
) where

newtype UserName = UserName { getUserName :: String
                            } deriving (Eq, Show)

newtype UUID = UUID { getUUID :: String
                    } deriving (Eq, Show)

data User = User { uniqueId :: UUID
                 , userName :: UserName
                 } deriving (Eq, Show)

class HasUser a where
  getUser :: a -> User

instance HasUser User where
  getUser user = user

fromStrings :: String -> String -> User
fromStrings uuid name = User (UUID uuid) (UserName name)
