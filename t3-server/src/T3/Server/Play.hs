module T3.Server.Play
  ( main
  ) where

newtype GameId = GameId String
  deriving (Show, Eq)

newtype UserId = UserId String
  deriving (Show, Eq)

newtype Token = Token String
  deriving (Show, Eq)

newtype Move = Move (Int, Int)
  deriving (Show, Eq)

type Step = ()

class Monad m => Client m where
  getUserId :: m UserId
  getToken :: m Token
  getMove :: m Move
  getGameId :: m GameId
  putStep :: Step -> m ()

class Monad m => Registry m where
  validateUser :: UserId -> Token -> m ()

class Monad m => Game m where
  submitMove :: GameId -> UserId -> Move -> m Step


main :: (Registry m, Client m, Game m) => m ()
main = do
  userId <- getUserId
  token <- getToken
  validateUser userId token
  move <- getMove
  gameId <- getGameId
  step <- submitMove gameId userId move
  putStep step
