module T3.Server.Dispatch.Class
  ( Dispatch(..)
  ) where

import T3.Server.Dispatch.Types

class Monad m => Dispatch m where
  forkMatch
    :: Maybe Seconds
    -> (UserName, MatchToken, Callback m)
    -> (UserName, MatchToken, Callback m)
    -> ([Action] -> Board -> Result -> m ())
    -> m ()
    -> m (MatchConfig m)
