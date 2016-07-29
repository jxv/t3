module T3.Server.MatchLoggerImpl
  ( logMatch
  ) where

import T3.Core (Result, Action, Board)
import T3.Server (MatchId, Users)
import T3.Server.Storage (Storage(..), Playback(..))
import T3.Server.Console (Console(..))
import T3.Server.MatchInstance (MatchInstance(..))

logMatch :: (Storage m, Console m, MatchInstance m) => [Action] -> Board -> Result -> m ()
logMatch actions board result = do
  matchId <- getMatchId
  users <- getUsers
  let playback :: Playback
      playback = Playback
        { _pbMatchId = matchId
        , _pbUsers = users
        , _pbActions = actions
        , _pbResult = result
        }
  storePlayback playback
  printStdout $ "Finished Game: " `mappend` ""
    
