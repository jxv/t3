module T3.Match.ConnectionCallback
  ( ConnectionCallback(..)
  ) where

import T3.Core (Loc, XO)
import T3.Match.Types (Step)

class Monad m => ConnectionCallback m where
  getRequest :: XO -> m (Loc, Step -> m ())
  getRespond :: XO -> m (Step -> m ())
  putRespond :: XO -> (Step -> m ()) -> m ()
