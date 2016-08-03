module T3.Match.ConnectionCallback
  ( ConnectionCallback(..)
  ) where

import T3.Core (Loc)
import T3.Match.Types (Step)
import T3.Match.Connection (Connection)

class Monad m => ConnectionCallback m where
  getRequest :: Connection -> m (Loc, Step -> m ())
  getRespond :: Connection -> m (Step -> m ())
  putRespond :: Connection -> (Step -> m ()) -> m ()
