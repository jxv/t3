module T3.Server.Class
  ( ServerEsque(..)
  ) where

import T3.Server.Types

class Monad m => ServerEsque m where
  registerUser :: RegisterRequest -> m (Maybe RegisterResponse)
  playMove :: MatchId -> MatchToken -> PlayRequest -> m (Maybe PlayResponse)
  startMatch :: StartRequest -> m (Maybe StartResponse)
  randomMatch :: StartRequest -> m (Maybe StartResponse)
