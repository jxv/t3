module T3.Server.Control.Play
  ( play
  ) where

import Control.Concurrent.Chan (readChan, writeChan)
import Control.Applicative ((<|>))
import Control.Monad (forever, unless)
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader, asks)
import Control.Monad.Except (ExceptT, MonadError(throwError))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Servant

import T3.Core (Loc)
import T3.Server.Types
import T3.Server.Control.Types

import T3.Server.Control.Lobby (Registry(..))

class Monad m => Game m where
  submitMove :: GameId -> UserId -> Loc -> m Step

play :: (Registry m, Game m) => PlayReq -> m PlayResp
play (PlayReq (Creds userId token) gameId loc) = do
  validateUser userId token
  step <- submitMove gameId userId loc
  return $ PlayResp (StepJSON step)

submitMove' :: (MonadIO m, MonadReader Env m) => GameId -> UserId -> Loc -> m Step
submitMove' gameId userId loc = do
  findGameObject <- asks (_gamesObjectFindGame . _envGamesObject)
  mgr <- liftIO $ findGameObject gameId
  case mgr of
    Nothing -> error "Can't find Game by GameId"
    Just (GameRecord _ (Pair gameObjectX gameObjectO)) -> do
      case gameObjectByUserId userId gameObjectX <|> gameObjectByUserId userId gameObjectO of
        Nothing -> error "Can't find User in Game by UserId"
        Just (sendLoc, recvStep) -> do
          liftIO $ sendLoc loc
          liftIO recvStep

gameObjectByUserId :: UserId -> LabeledGameObject -> Maybe (Loc -> IO (), IO Step)
gameObjectByUserId userId (LabeledGameObject userId' (GameObject f g)) =
  if userId == userId'
    then Just (writeChan f, readChan g)
    else Nothing

instance Game AppHandler where
  submitMove = submitMove'
