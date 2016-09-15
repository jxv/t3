module T3.Server.Register
  ( main
  ) where

newtype UserId = UserId String
  deriving (Show, Eq)

newtype Token = Token String
  deriving (Show, Eq)

class Monad m => Client m where
  getUserId :: m UserId
  putToken :: Token -> m ()

class Monad m => Registry m where
  createUser :: UserId -> m Token

main :: (Registry m, Client m) => m ()
main = do
  userId <- getUserId
  token <- createUser userId
  putToken token
