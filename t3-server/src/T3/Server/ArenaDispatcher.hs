{-# LANGUAGE TemplateHaskell #-}
module T3.Server.ArenaDispatcher
  ( main
  , step
  ) where

import Control.Lens
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, MonadReader, asks)
import Control.Monad.IO.Class (MonadIO(liftIO))

import T3.Server.Types (GameId, UserId)

class Monad m => Lobby m where
  popUserPair :: m (UserId, UserId)

class Monad m => Dispatch m where
  dispatchGame :: GameId -> UserId -> UserId -> m ()

class Monad m => Usher m where
  tellGameId :: UserId -> GameId -> m ()

class Monad m => GenId m where
  genGameId :: m GameId

main :: (Lobby m, Dispatch m, GenId m, Usher m) => m ()
main = forever step

step :: (Lobby m, Dispatch m, GenId m, Usher m) => m ()
step = do
  (xUserId, oUserId) <- popUserPair
  gameId <- genGameId
  dispatchGame gameId xUserId oUserId
  tellGameId xUserId gameId
  tellGameId oUserId gameId

data Env = Env
  { _envPopUserPair :: IO (UserId, UserId)
  , _envDispatch :: GameId -> UserId -> UserId -> IO ()
  , _envTellGameId :: UserId -> GameId -> IO ()
  , _envGenGameId :: IO GameId
  }

makeClassy ''Env

newtype ArenaDispatcher a = ArenaDispatcher (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

instance Lobby ArenaDispatcher where
  popUserPair = view envPopUserPair >>= liftIO

instance Dispatch ArenaDispatcher where
  dispatchGame g ux uo = view envDispatch >>= \f -> liftIO $ f g ux uo

instance Usher ArenaDispatcher where
  tellGameId u g = view envTellGameId >>= \f -> liftIO $ f u g

instance GenId ArenaDispatcher where
  genGameId = view envGenGameId >>= liftIO
