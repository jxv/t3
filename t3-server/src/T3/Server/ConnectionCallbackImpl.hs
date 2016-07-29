module T3.Server.ConnectionCallbackImpl
  ( getRequest
  , getRespond
  , putRespond
  ) where

import qualified Data.Map as Map ((!), insert)
import Data.Map (Map)

import T3.Core (Loc)
import T3.Server (Step)
import T3.Server.Connection (Connection)
import T3.Server.HasCallbacks (HasCallbacks(..))

getRequest :: HasCallbacks m => Connection -> m (Loc, Step -> m ())
getRequest connection = do
  callbacks <- getCallbacks
  fst $ callbacks Map.! connection

getRespond :: HasCallbacks m => Connection -> m (Step -> m ())
getRespond connection = do
  callbacks <- getCallbacks
  return . snd $ callbacks Map.! connection

putRespond :: HasCallbacks m => Connection -> (Step -> m ()) -> m ()
putRespond connection respond = do
  callbacks <- getCallbacks
  let callback = callbacks Map.! connection
  let callback' = (fst callback, respond)
  let callbacks' = Map.insert connection callback' callbacks
  putCallbacks callbacks'
