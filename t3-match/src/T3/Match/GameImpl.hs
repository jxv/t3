module T3.Match.GameImpl
  ( move
  , forfeit
  , end
  , tie
  ) where

import T3.Game.Game (Win(Win), Lose(Lose))
import T3.Core (XO(..), Loc(..), Result(..), Action(..), Board, yinYang)
import T3.Match.Types (Final(..))
import T3.Match.Communicator (Communicator(..))

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
