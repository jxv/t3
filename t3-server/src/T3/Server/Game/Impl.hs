module T3.Server.Game.Impl
  ( move
  , forfeit
  , end
  , tie
  , step
  ) where

import T3.Game (Win(..), Lose(..))
import T3.Core (XO(..), Loc(..), Result(..), Action(..), Board, yinYang)
import T3.Server (Final(..))
import T3.Server.Match (Match(..))

move :: Match m => XO -> m Loc
move xo = do
  sendGameState xo
  recvAction xo

forfeit :: Match m => Win XO -> Lose XO -> m ()
forfeit (Win w) (Lose l) = do
  tally (Winner w)
  sendFinal w WonByDQ
  sendFinal l LossByDQ

end :: Match m => Win XO -> Lose XO -> m ()
end (Win w) (Lose l) = do
  tally (Winner w)
  sendFinal w Won
  sendFinal l Loss

tie :: Match m => m ()
tie = do
  tally Tie
  sendFinal X Tied
  sendFinal O Tied

step :: Match m => Board -> XO -> Loc -> m ()
step b xo loc = do
  logAction xo loc
  updateBoard b
