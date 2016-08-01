{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module T3.Server.GameInstance
  (
  ) where

import qualified Control.Concurrent as IO
import qualified Control.Monad.STM as IO
import Control.Concurrent.Async (race)
import Control.Monad.State (StateT(..), MonadState(..), gets, evalStateT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Conc.Class (threadDelay)
import Data.Map (Map)

import T3.Game (Game(..))
import T3.Core (Loc, Action, Board)

import qualified T3.Server.ConsoleImpl as Console
import qualified T3.Server.GameImpl as Game
import qualified T3.Server.MatchImpl as Match
import qualified T3.Server.MatchTransmitterImpl as MatchTransmitter
import qualified T3.Server.ConnectionCallbackImpl as ConnectionCallback
import qualified T3.Server.MatchLoggerImpl as MatchLogger
import T3.Server.Connection (Connection)
import T3.Server.Milliseconds (Milliseconds(..))
import T3.Server (Step(..), Users, MatchId)
import T3.Server.Match (Match(..))
import T3.Server.MatchLogger (MatchLogger(..))
import T3.Server.HasMatchState (HasMatchState(..))
import T3.Server.HasConnection (HasConnection(..))
import T3.Server.MatchTransmitter (MatchTransmitter(..))
import T3.Server.ConnectionCallback (ConnectionCallback(..))
import T3.Server.Stoppable (Stoppable(..))
import T3.Server.HasCallbacks (HasCallbacks(..))
import T3.Server.OnTimeout (OnTimeout(..))
import T3.Server.HasTimeoutLimit (HasTimeoutLimit(..))
import T3.Server.Console (Console(..))
import T3.Server.Storage (Storage(..))
import T3.Server.MatchInfo (MatchInfo(..))

data GameState m = GameState
  { _gameStateCallbacks :: Callbacks m
  , _gameStateTimeoutLimit :: Maybe Milliseconds
  , _gameStateMatchInfo :: MatchInfoData
  , _gameStateMatchState :: MatchStateData
  }

data Callbacks m = Callbacks
  { _callbacksConnectionMap :: Map Connection (m (Loc, Step -> m ()), Step -> m ())
  }

data MatchInfoData = MatchInfoData
  { _matchInfoUsers :: Users
  , _matchInfoMatchId :: MatchId
  } deriving (Show, Eq)

data MatchStateData = MatchStateData
  { _matchStateBoard :: Board
  , _matchStateActions :: [Action]
  } deriving (Show, Eq)

newtype GameM a = GameM { unGameM :: StateT (GameState GameM) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (GameState GameM))

runGameM :: GameM a -> GameState GameM -> IO (a, GameState GameM)
runGameM game st = runStateT (unGameM game) st

instance Game GameM where
  move = Game.move

instance Match GameM where
  sendGameState = Match.sendGameState
  recvAction = Match.recvAction
  sendFinal = Match.sendFinal
  tally = Match.tally
  updateBoard = Match.updateBoard
  logAction = Match.logAction

instance Stoppable GameM where
  stop = undefined

instance HasMatchState GameM where
  getBoard = undefined
  putBoard = undefined
  getActions = undefined
  appendAction = undefined

instance HasConnection GameM where
  getConnection = undefined

instance MatchTransmitter GameM where
  sendStep = MatchTransmitter.sendStep
  recvLoc = MatchTransmitter.recvLoc

instance ConnectionCallback GameM where
  getRequest = ConnectionCallback.getRequest
  getRespond = ConnectionCallback.getRespond
  putRespond = ConnectionCallback.putRespond

instance HasCallbacks GameM where
  getCallbacks = gets (_callbacksConnectionMap . _gameStateCallbacks)
  putCallbacks connectionMap = do
    gameState <- get
    put $ gameState{ _gameStateCallbacks = (_gameStateCallbacks gameState){ _callbacksConnectionMap = connectionMap } }

instance MatchLogger GameM where
  logMatch = MatchLogger.logMatch

instance MatchInfo GameM where
  getUsers = gets (_matchInfoUsers . _gameStateMatchInfo)
  getMatchId = gets (_matchInfoMatchId . _gameStateMatchInfo)

instance OnTimeout GameM where
  onTimeout callee ms = do
    st <- get
    a <- liftIO $ race (liftIO $ delay 3000) (liftIO $ runGameM callee st)
    case a of
      Left _ -> return Nothing
      Right (b, st') -> do
        put st'
        return $ Just b
    where
      delay :: Milliseconds -> IO ()
      delay (Milliseconds ms) = threadDelay (scaleFromNano * ms)
        where
          scaleFromNano = 1000

instance HasTimeoutLimit GameM where
  getTimeoutLimit = gets _gameStateTimeoutLimit

instance Console GameM where
  printStdout = Console.printStdout

instance Storage GameM where
  storeUsers _ = return ()
  loadUsers = undefined
  loadMatchList = undefined
  storePlayback = undefined
  loadPlayback = undefined
