module T3.GameCallbacks.Finalizer
  ( Finalizer(..)
  ) where

import T3.Core (Board, Result, Action)

class Monad m => Finalizer m where
  finalize :: [Action] -> Board -> Result -> m ()
