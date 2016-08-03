module T3.Match.GameImpl
  ( move
  , forfeit
  , end
  , tie
  , step
  ) where

import T3.Game (Win(..), Lose(..))
import T3.Core (XO(..), Loc(..), Result(..), Action(..), Board, yinYang)
import T3.Match.Types (Final(..))
import T3.Match.GameComm (GameComm(..))

move :: GameComm m => XO -> m Loc
move xo = do
  sendGameState xo
  recvAction xo

forfeit :: GameComm m => Win XO -> Lose XO -> m ()
forfeit (Win w) (Lose l) = do
  tally (Winner w)
  sendFinal w WonByDQ
  sendFinal l LossByDQ

end :: GameComm m => Win XO -> Lose XO -> m ()
end (Win w) (Lose l) = do
  tally (Winner w)
  sendFinal w Won
  sendFinal l Loss

tie :: GameComm m => m ()
tie = do
  tally Tie
  sendFinal X Tied
  sendFinal O Tied

step :: GameComm m => Board -> XO -> Loc -> m ()
step b xo loc = do
  logAction xo loc
  updateBoard b
