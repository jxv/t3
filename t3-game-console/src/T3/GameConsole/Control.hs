module T3.GameConsole.Control
  ( move'
  , forfeit'
  , end'
  , tie'
  ) where

import Prelude hiding (putStrLn, getLine)
import Safe (readMay)

import T3.Core (Loc(..), XO(..), Board, boardList)
import T3.Game.Types (Win(Win), Lose(Lose))
import T3.Game.Control (Control(move))

import T3.GameConsole.Console (Console(putStrLn, getLine))

move' :: (Console m, Control m) => XO -> m Loc
move' xo = do
  putStrLn $ "Turn: " ++ show xo
  str <- getLine
  case readMay str of
    Nothing -> do
      putStrLn "Try again."
      move xo
    Just (x,y) ->
        return $ Loc x y

forfeit' :: Console m => Win XO -> Lose XO -> m ()
forfeit' (Win w) (Lose l) = do
  putStrLn $ show l ++ " forfeited."
  putStrLn $ show w ++ " won."

end' :: Console m => Win XO -> Lose XO -> m ()
end' (Win w) (Lose l) = do
  putStrLn $ show w ++ " won, and " ++ show l ++ " lost."

tie' :: Console m => m ()
tie' = putStrLn "Tie game!"
