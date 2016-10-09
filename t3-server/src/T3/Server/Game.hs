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
import T3.Core (emptyBoard, Board, XO(..), Loc(..), Result, yinYang)
import T3.Game.Play (Play(..), play')
import T3.Game.Control (Control(..))
import T3.Game.BoardManager (BoardManager(..), isOpenLoc_, insertAtLoc_, getResult_)
import T3.Game.Types (Win(..), Lose(..), Step(..), Final(..))
import T3.Bot.Random (randomLoc)
import T3.Server.Types

import T3.Server.PracticeDispatcher (botId)

class Monad m => Exit m where
  exit :: m ()

class Monad m => HasGameStart m where
  getGameStart :: m GameStart

class Monad m => Bot m where
  botMove :: Board -> m Loc

class Monad m => HasBoard m where
  getBoard :: m Board
  putBoard :: Board -> m ()

class Monad m => Initial m where
  initialStep :: Step -> m ()

class Monad m => Self m where
  killSelf :: m ()
  removeSelf :: m ()

class Monad m => Communicator m where
  recvLoc :: XO -> m Loc
  sendStep :: XO -> Step -> m ()

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

move' :: (Bot m, HasBoard m, Exit m, Communicator m, HasGameStart m) => XO -> m Loc
move' xo = do
  userId <- byUser _gameStartX _gameStartO xo <$> getGameStart
  if userId == botId
    then botMove =<< getBoard
    else recvLoc xo

botMove' :: (Exit m, MonadIO m) => Board -> m Loc
botMove' board = do
  maybeLoc <- liftIO $ randomLoc board
  case maybeLoc of
    Nothing -> exit >> error "Bot can't move"
    Just loc -> return loc

forfeit' :: (Communicator m, Exit m, HasBoard m) => Win XO -> Lose XO -> m ()
forfeit' (Win w) (Lose l) = do
  board <- getBoard
  sendStep w $ Step board (Just WonByDQ)
  sendStep l $ Step board (Just LossByDQ)
  exit

end' :: (Communicator m, Exit m, HasBoard m) => Win XO -> Lose XO -> m ()
end' (Win w) (Lose l) = do
  board <- getBoard
  sendStep w $ Step board (Just Won)
  sendStep l $ Step board (Just Loss)
  exit

tie' :: (Communicator m, Exit m, HasBoard m) => m ()
tie' = do
  board <- getBoard
  let step = Step board (Just Tied)
  sendStep X step
  sendStep O step
  exit

gameObj :: XO -> Env -> GameCb
gameObj = byUser _envGameCbX _envGameCbO

recvLoc' :: (MonadReader Env m, MonadIO m) => XO -> m Loc
recvLoc' xo = do
  f <- asks (readChan . fst . gameObj xo)
  liftIO f

sendStep' :: (MonadReader Env m, MonadIO m) => XO -> Step -> m ()
sendStep' xo step = do
  f <- asks (writeChan . snd . gameObj xo)
  liftIO $ f step

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
  cb <- asks (writeChan . snd . gameObj (yinYang xo))
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

instance Communicator Game where
  recvLoc = recvLoc'
  sendStep = sendStep'

instance Bot Game where
  botMove = botMove'

instance HasGameStart Game where
  getGameStart = asks _envGameStart
