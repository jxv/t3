module T3.Server.Part
  ( Part(..)
  ) where

import qualified Data.Map as M

import T3.Server (StartRequest, StartResponse, PlayRequest, PlayResponse, UserConfig, MatchId, MatchToken)

class Monad m => Part m where
  randomResponse :: StartRequest -> m StartResponse
  playResponse :: UserConfig m -> PlayRequest -> m PlayResponse
  userConfig :: MatchId -> MatchToken -> PlayRequest -> m (UserConfig m)
