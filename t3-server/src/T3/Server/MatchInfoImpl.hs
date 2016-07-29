module T3.Server.MatchInfoImpl
  ( getUsers
  , getMatchId
  , MatchInfoReader(..)
  ) where

import Control.Monad.Reader (MonadReader, asks)
import T3.Server (Users, MatchId)

data MatchInfoReader = MatchInfoReader
  { _matchInfoReaderUsers :: Users
  , _matchInfoReaderMatchId :: MatchId
  } deriving (Show, Eq)

getUsers :: MonadReader MatchInfoReader m => m Users
getUsers = asks _matchInfoReaderUsers

getMatchId :: MonadReader MatchInfoReader m => m MatchId
getMatchId = asks _matchInfoReaderMatchId
