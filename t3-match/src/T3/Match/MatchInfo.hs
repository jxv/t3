module T3.Match.MatchInfo
  ( MatchInfo(..)
  ) where

import T3.Match.Types (Users, MatchId)

class Monad m => MatchInfo m where
  getUsers :: m Users
  getMatchId :: m MatchId
