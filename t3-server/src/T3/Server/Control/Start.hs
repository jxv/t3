module T3.Server.Control.Start
  ( start
  ) where

import Control.Concurrent.Chan (readChan)
import Control.Applicative ((<|>))
import Control.Monad (forever, unless)
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader, asks)
import Control.Monad.Except (ExceptT, MonadError(throwError))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Servant

import T3.Core (Loc)
import T3.Server.Types
import T3.Server.Control.Types

import T3.Server.Control.PracticeLobby (Registry(..))

class Monad m => Game m where
  getStart :: GameId -> m GameStart
  getStep :: GameId -> UserId -> m Step

start :: (Registry m, Game m) => StartReq -> m StartResp
start (StartReq (Creds userId token) gameId) = do
  validateUser userId token
  gameStart <- getStart gameId
  step <- getStep gameId userId
  return $ StartResp (StepJSON step) gameStart

getStart' :: (MonadIO m, MonadReader Env m) => GameId -> m GameStart
getStart' gameId = do
  findGameCb <- asks (_gamesCbFindGame . _envGamesCb)
  maybeGameRec <- liftIO $ findGameCb gameId
  case maybeGameRec of
    Nothing -> error "Can't find Game by GameId"
    Just (_, (userX, _), (userO, _)) -> return $ GameStart gameId userX userO

getStep' :: (MonadIO m, MonadReader Env m) => GameId -> UserId -> m Step
getStep' gameId userId = do
  findGameCb <- asks (_gamesCbFindGame . _envGamesCb)
  maybeGameRec <- liftIO $ findGameCb gameId
  case maybeGameRec of
    Nothing -> error "Can't find Game by GameId"
    Just (_, gameCbX, gameCbO) -> do
      case locByUserId userId gameCbX <|> locByUserId userId gameCbO of
        Nothing -> error "Can't find User in Game by UserId"
        Just recvStep -> liftIO recvStep

locByUserId :: UserId -> (UserId, GameCb) -> Maybe (IO Step)
locByUserId userId (userId', (_, g)) =
  if userId == userId'
    then Just $ readChan g
    else Nothing

instance Game AppHandler where
  getStart = getStart'
  getStep = getStep'
