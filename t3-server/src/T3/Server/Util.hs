module T3.Server.Util
  ( toGameState
  , genUserKey
  , genMatchToken
  , genMatchId
  , genUserName
  ) where

import Control.Monad.Random (MonadRandom(..))

import T3.Util
import T3.Server.Types
import T3.Match.Types

toGameState :: Step -> GameState
toGameState s = GameState (_stepBoard s) (_stepFinal s)

genUserKey :: MonadRandom m => m UserKey
genUserKey = UserKey <$> genBase64 32

genMatchToken :: MonadRandom m => m MatchToken
genMatchToken = MatchToken <$> genBase64 16

genMatchId :: MonadRandom m => m MatchId
genMatchId = MatchId <$> genBase64 16

genUserName :: MonadRandom m => m UserName
genUserName = UserName <$> genBase64 32
