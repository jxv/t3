module T3.Game.GameState
  ( GameStateT
  , runGameStateT
  ) where

import Control.Monad.State (MonadState(..), StateT, evalStateT)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO)
import T3.Core (Board)
import T3.Game.Types
import T3.Game.HasBoard

newtype GameStateT m a = GameStateT { unGameStateT :: StateT Board m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadState Board, MonadIO)

runGameStateT :: Monad m => GameStateT m a -> Board -> m a
runGameStateT game board = evalStateT (unGameStateT game) board

instance Monad m => HasBoard (GameStateT m) where
  getBoard = get
  putBoard = put
