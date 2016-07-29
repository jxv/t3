module T3.Server.MatchInstanceImpl
  ( getUsers
  , getMatchId
  ) where

import Control.Monad.Reader (MonadReader, asks)
import T3.Server (Users, MatchId)

data MatchInstanceReader = MatchInstanceReader
  { _matchInstanceReaderUsers :: Users
  , _matchInstanceReaderMatchId :: MatchId
  } deriving (Show, Eq)

getUsers :: MonadReader MatchInstanceReader m => m Users
getUsers = asks _matchInstanceReaderUsers

getMatchId :: MonadReader MatchInstanceReader m => m MatchId
getMatchId = asks _matchInstanceReaderMatchId
