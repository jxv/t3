module T3.Comm.Game where

import Prelude
import Data.Text (Text)
import T3.Game
import T3.Comm.Class
import T3.Comm.Types

move' :: Comm m => XO -> m Loc
move' xo = do
  sendGameState xo
  recvMove xo

forfeit' :: Comm m => Win XO -> Lose XO -> m ()
forfeit' (Win w) (Lose l) = do
  sendFinal l LossByDQ
  sendFinal w WonByDQ

end' :: Comm m => Win XO -> Lose XO -> m ()
end' (Win w) (Lose l) = do
  sendFinal w Won
  sendFinal l Loss

tie' :: Comm m => m ()
tie' = do
  sendFinal X Tied
  sendFinal O Tied

step' :: Comm m => Board -> m ()
step' b = do
  updateBoard b
