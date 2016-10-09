module T3.Server.Game
  ( Game
  , Env(..)
  , run
  , main
  ) where

import Control.Concurrent.Chan (readChan, writeChan)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(..), asks)
import Control.Monad.State (StateT(..), evalStateT, MonadState(..))

import qualified T3.Game.Main as Game (main)
import T3.Core (emptyBoard, Board, XO(..), Loc, Result, yinYang)
import T3.Game.Play (Play(..), play')
import T3.Game.Control (Control(..))
import T3.Game.BoardManager (BoardManager(..), isOpenLoc_, insertAtLoc_, getResult_)
import T3.Game.Types (Win(..), Lose(..), Step(..))
import T3.Server.Types

class Monad m => Console m where
  display :: String -> m ()

class Monad m => Exit m where
  exit :: m ()

class Monad m => HasBoard m where
  getBoard :: m Board
  putBoard :: Board -> m ()

class Monad m => Initial m where
  initialStep :: Step -> m ()

class Monad m => Self m where
  killSelf :: m ()
  removeSelf :: m ()

main :: (Initial m, HasBoard m, Play m) => m ()
main = do
  board <- getBoard
  let step = Step board Nothing
  initialStep step
  Game.main

initialStep' :: (MonadIO m, MonadReader Env m) => Step -> m ()
initialStep' step = do
  (_, stepChan) <- asks _envGameCbX
  liftIO $ writeChan stepChan step

move' :: (MonadReader Env m, MonadIO m) => XO -> m Loc
move' xo = do
  cb <- asks (readChan . fst . (case xo of X -> _envGameCbX; O -> _envGameCbO))
  liftIO cb

forfeit' :: (MonadReader Env m, Console m, Exit m) => Win XO -> Lose XO -> m ()
forfeit' (Win w) (Lose l) = do
  gs <- asks _envGameStart
  display $ "[forfeit] W " ++ show w ++ " - L " ++ show l
  display $ show gs
  exit

end' :: (MonadReader Env m, Console m, Exit m) => Win XO -> Lose XO -> m ()
end' (Win w) (Lose l) = do
  gs <- asks _envGameStart
  display $ "[end] W " ++ show w ++ " - L " ++ show l
  display $ show gs
  exit

tie' :: (MonadReader Env m, Console m, Exit m) => m ()
tie' = do
  gs <- asks _envGameStart
  display "[tie]"
  display $ show gs
  exit

exit' :: Self m => m ()
exit' = killSelf >> removeSelf

removeSelf' :: (MonadIO m, MonadReader Env m) => m ()
removeSelf' = do
  env <- ask
  let games = _envGamesCb env
  let gameId = _gameStartGameId $ _envGameStart env
  liftIO $ _gamesCbRemoveGame games gameId

killSelf' :: (MonadIO m, MonadReader Env m) => m ()
killSelf' = do
  env <- ask
  let games = _envGamesCb env
  let gameId = _gameStartGameId $ _envGameStart env
  maybeGameRec <- liftIO $ _gamesCbFindGame games gameId
  case maybeGameRec of
    Just (threadCb, _, _) -> liftIO $ _threadCbKill threadCb
    Nothing -> return ()

isOpenLoc' :: HasBoard m => Loc -> m Bool
isOpenLoc' loc = do
  board <- getBoard
  return $ isOpenLoc_ loc board

insertAtLoc' :: (MonadIO m, MonadReader Env m, HasBoard m) => Loc -> XO -> m ()
insertAtLoc' loc xo = do
  board <- getBoard
  let board' = insertAtLoc_ loc xo board
  putBoard board'
  let step = Step board' Nothing
  cb <- asks (writeChan . snd . (case (yinYang xo) of X -> _envGameCbX; O -> _envGameCbO))
  liftIO $ cb step

getResult' :: HasBoard m => m Result
getResult' = do
  board <- getBoard
  return $ getResult_ board

data Env = Env
  { _envResultsCb :: ResultsCb
  , _envGamesCb :: GamesCb
  , _envGameStart :: GameStart
  , _envGameCbX :: GameCb
  , _envGameCbO :: GameCb
  }

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

instance Console Game where
  display = liftIO . putStrLn

instance Exit Game where
  exit = exit'

instance HasBoard Game where
  getBoard = get
  putBoard = put

instance Self Game where
  killSelf = killSelf'
  removeSelf = removeSelf'

instance Initial Game where
  initialStep = initialStep'
