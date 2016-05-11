module T3.Server.Dispatch where

import T3.Game
import T3.Match

data UserConfig m = UserConfig
  { _userCfgUserName :: UserName
  , _userCfgMatchToken :: MatchToken
  , _userCfgSendLoc :: (Loc, Callback m) -> m ()
  }

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
