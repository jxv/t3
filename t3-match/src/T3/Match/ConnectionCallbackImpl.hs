module T3.Match.ConnectionCallbackImpl
  ( getRequest
  , getRespond
  , putRespond
  ) where

import qualified Data.Map as Map ((!), insert)
import Data.Map (Map)

import T3.Core (Loc, XO)
import T3.Match.Types (Step)
import T3.Match.HasCallbacks (HasCallbacks(..))

getRequest :: HasCallbacks m => XO -> m (Loc, Step -> m ())
getRequest xo = do
  callbacks <- getCallbacks
  fst $ callbacks Map.! xo

getRespond :: HasCallbacks m => XO -> m (Step -> m ())
getRespond xo = do
  callbacks <- getCallbacks
  return . snd $ callbacks Map.! xo

putRespond :: HasCallbacks m => XO -> (Step -> m ()) -> m ()
putRespond xo respond = do
  callbacks <- getCallbacks
  let callback = callbacks Map.! xo
  let callback' = (fst callback, respond)
  let callbacks' = Map.insert xo callback' callbacks
  putCallbacks callbacks'
