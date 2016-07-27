module T3.Server.Connection
  ( Connection
  , mkConnection
  ) where

import T3.Server (MatchId, UserName)

newtype Connection = Connection (MatchId, UserName)
  deriving (Show, Eq, Ord)

mkConnection :: MatchId -> UserName -> Connection
mkConnection matchId userName = Connection (matchId, userName)
