module T3.Server.ArenaDispatcher
  ( main
  , step
  ) where

import Control.Monad (forever)

newtype GameId = GameId String
  deriving (Show, Eq)

newtype UserId = UserId String
  deriving (Show, Eq)

class Monad m => Lobby m where
  popUserPair :: m (UserId, UserId)

class Monad m => Dispatch m where
  dispatchGame :: GameId -> UserId -> UserId -> m ()

class Monad m => Usher m where
  tellGameId :: UserId -> GameId -> m ()

class Monad m => GenId m where
  genGameId :: m GameId

main :: (Lobby m, Dispatch m, GenId m, Usher m) => m ()
main = forever step

step :: (Lobby m, Dispatch m, GenId m, Usher m) => m ()
step = do
  (xUserId, oUserId) <- popUserPair
  gameId <- genGameId
  dispatchGame gameId xUserId oUserId
  tellGameId xUserId gameId
  tellGameId oUserId gameId
