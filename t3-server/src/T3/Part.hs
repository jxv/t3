module T3.Part
  ( Part(..)
  ) where

import qualified Data.Map as M

import T3.Server hiding (ServerEsque(..)) -- types
import T3.Match hiding (Match(..), MatchToken) -- types

class Monad m => Part m where
  tryRegister :: UserName -> UserKey -> m (Maybe (M.Map UserName UserKey))
  authenticate :: UserCreds -> m Bool
  randomResponse :: StartRequest -> m StartResponse
  playResponse :: UserConfig m -> PlayRequest -> m (Maybe PlayResponse)
  userConfig :: MatchId -> MatchToken -> PlayRequest -> m (Maybe (UserConfig m))
