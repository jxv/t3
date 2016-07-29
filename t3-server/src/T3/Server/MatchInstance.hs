module T3.Server.MatchInstance
  ( MatchInstance(..)
  ) where

import T3.Server (Users, MatchId)

class Monad m => MatchInstance m where
  getUsers :: m Users
  getMatchId :: m MatchId
