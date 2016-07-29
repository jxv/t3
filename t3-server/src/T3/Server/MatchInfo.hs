module T3.Server.MatchInfo
  ( MatchInfo(..)
  ) where

import T3.Server (Users, MatchId)

class Monad m => MatchInfo m where
  getUsers :: m Users
  getMatchId :: m MatchId
