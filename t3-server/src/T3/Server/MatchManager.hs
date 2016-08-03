module T3.Server.MatchManager
  ( MatchManager(..)
  ) where

import T3.Core (Loc)
import T3.Server (MatchId, UserName, Users)

class MatchManager m where
  dispatchMatch :: UserName -> UserName -> m MatchId
  dispatchMatchWithRandomBot :: UserName -> m MatchId
  getUsers :: MatchId -> m Users
  sendLocation :: MatchId -> UserName -> Loc -> m ()
  killMatch :: MatchId -> m ()
