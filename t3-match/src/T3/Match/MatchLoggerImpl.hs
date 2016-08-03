module T3.Match.MatchLoggerImpl
  ( logMatch
  ) where

import Data.Text (pack)
import T3.Core (Result, Action, Board)
import T3.Match.Types (MatchId, Users)
import T3.Match.Console (Console(..))
import T3.Match.MatchInfo (MatchInfo(..))

logMatch :: (Console m, MatchInfo m) => [Action] -> Board -> Result -> m ()
logMatch actions board result = do
  matchId <- getMatchId
  users <- getUsers
  printStdout $ "Finished Game:"
  printStdout . pack . show $ (matchId, users, actions, result)
    
