{-# LANGUAGE TemplateHaskell #-}
module T3.Server.PracticeDispatcher
  ( main
  , step
  ) where

import Control.Lens
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, MonadReader, asks)
import Control.Monad.IO.Class (MonadIO(liftIO))

import T3.Server.Types (GameId, UserId)

class Monad m => Lobby m where
  popUser :: m UserId

class Monad m => Dispatch m where
  dispatchGame :: GameId -> UserId -> m ()

class Monad m => Usher m where
  tellGameId :: UserId -> GameId -> m ()

class Monad m => GenId m where
  genGameId :: m GameId

botId :: UserId
botId = "random"

main :: (Lobby m, Dispatch m, GenId m, Usher m) => m ()
main = forever step

step :: (Lobby m, Dispatch m, GenId m, Usher m) => m ()
step = do
  userId <- popUser
  gameId <- genGameId
  dispatchGame gameId userId
  tellGameId userId gameId

data Env = Env
  { _envUserId :: IO UserId
  , _envDispatch :: GameId -> UserId -> IO ()
  , _envTellGameId :: UserId -> GameId -> IO ()
  , _envGenGameId :: IO GameId
  }

makeClassy ''Env

newtype PracticeDispatcher a = PracticeDispatcher (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

instance Lobby PracticeDispatcher where
  popUser = view envUserId >>= liftIO

instance Dispatch PracticeDispatcher where
  dispatchGame g u = view envDispatch >>= \f -> liftIO $ f g u

instance Usher PracticeDispatcher where
  tellGameId u g = view envTellGameId >>= \f -> liftIO $ f u g

instance GenId PracticeDispatcher where
  genGameId = view envGenGameId >>= liftIO
