module T3.Game.Monad
  ( GameT
  , runGameT
  ) where

import Control.Monad.State (MonadState(..), StateT, evalStateT)
import Control.Monad.IO.Class (MonadIO)

import T3.Core (Board)

import T3.Game.Types
import T3.Game.Classes

newtype GameT m a = GameT { unGameT :: StateT Board m a }
  deriving (Functor, Applicative, Monad, MonadState Board, MonadIO)

runGameT :: Monad m => GameT m a -> Board -> m a
runGameT game board = evalStateT (unGameT game) board

instance Monad m => HasBoard (GameT m) where
  getBoard = get
  putBoard = put
