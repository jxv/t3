module T3.Server.Start
  ( main
  ) where

newtype UserId = UserId String
  deriving (Show, Eq)

newtype Token = Token String
  deriving (Show, Eq)

newtype Ticket = Ticket String
  deriving (Show, Eq)

newtype GameId = GameId String
  deriving (Show, Eq)

class Monad m => Lobby m where
  enterLobby :: UserId -> m Ticket

class Monad m => Usher m where
  enterGame :: Ticket -> m GameId

class Monad m => Registry m where
  validateUser :: UserId -> Token -> m ()

class Monad m => Client m where
  getUserId :: m UserId
  getToken :: m Token
  sendGameId :: GameId -> m ()

main :: (Lobby m, Usher m, Registry m, Client m) => m ()
main = do
  userId <- getUserId
  token <- getToken
  validateUser userId token
  ticket <- enterLobby userId
  gameId <- enterGame ticket
  sendGameId gameId
