module T3.Server.Dispatch
  ( MatchConfig(..)
  , Dispatch(..)
  ) where

import T3.Core (Loc(..), Board, Result(..), Action(..))
import T3.Server -- types
import T3.Server.Match hiding (MatchToken) -- types

data MatchConfig m = MatchConfig
  { _matchCfgX :: UserConfig m
  , _matchCfgO :: UserConfig m
  , _matchCfgDie :: m ()
  }

class Monad m => Dispatch m where
  forkMatch
    :: Maybe Seconds
    -> (UserName, MatchToken, Callback m)
    -> (UserName, MatchToken, Callback m)
    -> ([Action] -> Board -> Result -> m ())
    -> m ()
    -> m (MatchConfig m)
