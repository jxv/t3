module T3.Server.Dispatch.Types
  ( Seconds(..)
  , UserConfig(..)
  , MatchConfig(..)
  , UserName(..)
  , MatchToken(..)
  , Loc(..)
  , Callback
  , Action(..)
  , Board
  , Result(..)
  ) where

import T3.Server.Types
import T3.Game
import T3.Match

data MatchConfig m = MatchConfig
  { _matchCfgX :: UserConfig m
  , _matchCfgO :: UserConfig m
  , _matchCfgDie :: m ()
  }