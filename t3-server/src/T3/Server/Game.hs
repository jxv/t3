module T3.Server.Game
  ( Game
  , Env(..)
  , run
  , main
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))

import T3.Server.Types

main :: Monad m => m ()
main = return ()

data Env = Env
  { _envResultsCb :: ResultsCb
  , _envGameStart :: GameStart
  , _envGameCbX :: GameCb
  , _envGameCbO :: GameCb
  }

newtype Game a = Game (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

run :: MonadIO m => Game a -> Env -> m a
run (Game m) env = liftIO $ m
