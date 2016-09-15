module T3.Server.Result
  ( main
  ) where

newtype GameId = GameId String
  deriving (Show, Eq)

newtype Playback = Playback ()
  deriving (Show, Eq)

class Monad m => Client m where
  getGameId :: m GameId
  putPlayback :: Playback -> m ()

class Monad m => Playbacks m where
  getPlayback :: GameId -> m Playback

main :: (Playbacks m, Client m) => m ()
main = do
  gameId <- getGameId
  playback <- getPlayback gameId
  putPlayback playback
