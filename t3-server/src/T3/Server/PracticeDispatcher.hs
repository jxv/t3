module T3.Server.PracticeDispatcher
  ( main
  , step
  ) where

import Control.Monad (forever)

newtype GameId = GameId String
  deriving (Show, Eq)

newtype UserId = UserId String
  deriving (Show, Eq)

class Monad m => Lobby m where
  popUser :: m UserId

class Monad m => Dispatch m where
  dispatchGame :: GameId -> UserId -> UserId -> m ()

class Monad m => Usher m where
  tellGameId :: UserId -> GameId -> m ()

class Monad m => GenId m where
  genGameId :: m GameId

botId :: UserId
botId = UserId "random"

main :: (Lobby m, Dispatch m, GenId m, Usher m) => m ()
main = forever step

step :: (Lobby m, Dispatch m, GenId m, Usher m) => m ()
step = do
  userId <- popUser
  gameId <- genGameId
  dispatchGame gameId userId botId
  tellGameId userId gameId
