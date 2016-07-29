{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module T3.Server.GameInstance
  (
  ) where

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.IO.Class (MonadIO(..))

import qualified T3.Server.GameImpl as Game
import T3.Game (Game(..))

newtype GameM a = GameM { unGameM :: ReaderT () IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
