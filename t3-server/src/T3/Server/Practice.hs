{-# LANGUAGE TemplateHaskell #-}
module T3.Server.Practice
  ( main
  ) where

import Control.Lens
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, MonadReader, asks)
import Control.Monad.IO.Class (MonadIO(liftIO))

import T3.Server.Types (UserId, Ticket, GameId, Token)

class Monad m => Lobby m where
  enterLobby :: UserId -> m Ticket

class Monad m => Usher m where
  enterGame :: Ticket -> m GameId

class Monad m => Registry m where
  validateUser :: UserId -> m ()

class Monad m => Client m where
  getUserId :: m UserId
  sendGameId :: GameId -> m ()

main :: (Lobby m, Usher m, Registry m, Client m) => m ()
main = do
  userId <- getUserId
  validateUser userId
  ticket <- enterLobby userId
  gameId <- enterGame ticket
  sendGameId gameId

data Env = Env
  { _envEnterLobby :: UserId -> IO Ticket
  }

makeClassy ''Env

newtype Practice a = Practice (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

instance Lobby Practice where
  enterLobby u = view envEnterLobby >>= \f -> liftIO $ f u
