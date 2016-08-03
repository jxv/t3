module T3.Match.Connection
  ( Connection
  , mkConnection
  ) where

import T3.Match.Types (MatchId, UserName)

newtype Connection = Connection (MatchId, UserName)
  deriving (Show, Eq, Ord)

mkConnection :: MatchId -> UserName -> Connection
mkConnection matchId userName = Connection (matchId, userName)
