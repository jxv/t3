module T3.Storage.Class where

import qualified Data.Map as M

import T3.Server.Types
import T3.Server.Dispatch.Types
import T3.Server.Lobby.Types
import T3.Match.Types
import T3.Storage.Types

class Monad m => Storage m where
  storeUsers :: M.Map UserName UserKey -> m ()
  loadUsers :: m (M.Map UserName UserKey)
  loadMatchList :: m [MatchId]
  storePlayback :: Playback -> m ()
  loadPlayback :: MatchId -> m (Maybe Playback)
