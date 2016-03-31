{-# OPTIONS -fno-warn-orphans #-}
module T3.DB where

import qualified Data.Map as M
import T3.Server
import T3.Server.Dispatch
import T3.Server.Lobby
import T3.Match
import T3.Random
import T3.Playback
import T3.Game.Core

class Monad m => DB m where
  storeUsers :: M.Map UserName UserKey -> m ()
  loadUsers :: m (M.Map UserName UserKey)
  loadMatchList :: m [MatchId]
  storePlayback :: Playback -> m ()
  loadPlayback :: MatchId -> m (Maybe Playback)
