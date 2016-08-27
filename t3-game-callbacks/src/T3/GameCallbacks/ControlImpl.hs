module T3.GameCallbacks.ControlImpl
  ( move
  , forfeit
  , end
  , tie
  ) where

import T3.Core (XO(X,O), Loc, Result(..), Action(..))
import T3.Game.Types (Win(Win), Lose(Lose))

import T3.GameCallbacks.Types (Final(..))
import T3.GameCallbacks.Parts (Communicator(..))

move :: Communicator m => XO -> m Loc
move xo = do
  sendGameState xo
  recvAction xo

forfeit :: Communicator m => Win XO -> Lose XO -> m ()
forfeit (Win w) (Lose l) = do
  tally (Winner w)
  sendFinal w WonByDQ
  sendFinal l LossByDQ

end :: Communicator m => Win XO -> Lose XO -> m ()
end (Win w) (Lose l) = do
  tally (Winner w)
  sendFinal w Won
  sendFinal l Loss

tie :: Communicator m => m ()
tie = do
  tally Tie
  sendFinal X Tied
  sendFinal O Tied
