module T3.Server.Main
  ( main
  ) where

type Lobby = ()

type Usher = ()

type Games = ()

type Results = ()

type Registry = ()

class Monad m => SharedState m where
  newLobby :: m Lobby
  newUsher :: m Usher
  newGames :: m Games
  newResults :: m Results
  newRegistry :: m Registry

class Monad m => Interthread m where
  fork :: m () -> m ()

class Monad m => Threads m where
  arenaDispatcher :: Lobby -> Usher -> Games -> m () -> m ()
  practiceDispatcher :: Lobby -> Usher -> Games -> m () -> m ()
  game :: m ()
  control :: Lobby -> Usher -> Games -> Results -> Registry -> m ()

main :: (SharedState m, Interthread m, Threads m) => m ()
main = do
  lobby <- newLobby
  usher <- newUsher
  games <- newGames
  results <- newResults
  registry <- newRegistry
  fork $ practiceDispatcher lobby usher games (fork game)
  fork $ arenaDispatcher lobby usher games (fork game)
  control lobby usher games results registry
