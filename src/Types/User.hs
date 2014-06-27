module Types.User
( UserName (..)
, UUID (..)
, User (..)
, HasUser (..)
) where

newtype UserName = UserName { getUserName :: String
                            } deriving (Eq, Show)

newtype UUID = UUID { getUUID :: String
                    } deriving (Eq, Show)

data User = User { userName :: UserName
                 , uniqueId :: UUID
                 } deriving (Eq, Show)

class HasUser a where
  getUser :: a -> User

instance HasUser User where
  getUser user = user
