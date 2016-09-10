module T3.Game.Control
  ( Control(..)
  ) where

import Data.Text (Text)
import T3.Core (XO, Loc, Board)
import T3.Game.Types

class Monad m => Control m where
  move :: XO -> m Loc
  forfeit :: Win XO -> Lose XO -> m ()
  end :: Win XO -> Lose XO -> m ()
  tie :: m ()
