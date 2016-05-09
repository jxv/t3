module T3.ServerLang
  ( ServerEsque(..)
  ) where

import T3.Server
import T3.Server.Dispatch
import T3.Server.Lobby
import T3.DB
import T3.Match
import T3.Random
import T3.Game.Core

class Monad m => ServerEsque m where
  registerUser :: RegisterRequest -> m (Maybe RegisterResponse)
  playMove :: MatchId -> MatchToken -> PlayRequest -> m (Maybe PlayResponse)
  startMatch :: StartRequest -> m (Maybe StartResponse)
  randomMatch :: StartRequest -> m (Maybe StartResponse)
