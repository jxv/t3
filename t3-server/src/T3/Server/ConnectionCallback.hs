module T3.Server.ConnectionCallback
  ( ConnectionCallback(..)
  ) where

import T3.Core (Loc)
import T3.Server (Step)
import T3.Server.Connection (Connection)

class Monad m => ConnectionCallback m where
  getRequest :: Connection -> m (Loc, Step -> m ())
  getRespond :: Connection -> m (Step -> m ())
  putRespond :: Connection -> (Step -> m ()) -> m ()
