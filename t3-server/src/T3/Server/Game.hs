module T3.Server.Game
  ( Game
  , Env(..)
  , run
  , main
  ) where

import Control.Concurrent.Chan (readChan)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(..), asks)
import Control.Monad.State (StateT(..), evalStateT, MonadState(..))

import T3.Core (emptyBoard, Board, XO(..), Loc, Result)
import T3.Game.Main (main)
import T3.Game.Play (Play(..), play')
import T3.Game.Control (Control(..))
import T3.Game.BoardManager (BoardManager(..), isOpenLoc_, insertAtLoc_, getResult_)
import T3.Game.Types (Win(..), Lose(..))
import T3.Server.Types

data Env = Env
  { _envResultsCb :: ResultsCb
  , _envGamesCb :: GamesCb
  , _envGameStart :: GameStart
  , _envGameCbX :: GameCb
  , _envGameCbO :: GameCb
  }

move' :: (MonadReader Env m, MonadIO m) => XO -> m Loc
move' xo = do
  cb <- asks (fst . (case xo of X -> _envGameCbX; O -> _envGameCbO))
  liftIO $ readChan cb

forfeit' :: Monad m => Win XO -> Lose XO -> m ()
forfeit' = undefined

end' :: Monad m => Win XO -> Lose XO -> m ()
end' = undefined

tie' :: Monad m => m ()
tie' = undefined

isOpenLoc' :: MonadState Board m => Loc -> m Bool
isOpenLoc' loc = do
  board <- get
  return $ isOpenLoc_ loc board

insertAtLoc' :: MonadState Board m => Loc -> XO -> m ()
insertAtLoc' loc xo = do
  board <- get
  put $ insertAtLoc_ loc xo board

getResult' :: MonadState Board m => m Result
getResult' = do
  board <- get
  return $ getResult_ board

newtype Game a = Game (StateT Board (ReaderT Env IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadState Board)

run :: MonadIO m => Game a -> Env -> m a
run (Game m) env = liftIO $ runReaderT (evalStateT m emptyBoard) env

instance Play Game where
  play = play'

instance Control Game where
  move = move'
  forfeit = forfeit'
  end = end'
  tie = tie'

instance BoardManager Game where
  isOpenLoc = isOpenLoc'
  insertAtLoc = insertAtLoc'
  getResult = getResult'
