module T3.Game.ConsoleImpl
  ( move
  , forfeit
  , end
  , tie
  , step
  ) where

import Prelude
import Safe (readMay)

import T3.Core (Loc(..), XO(..), Board, boardList)
import T3.Game hiding (Game(..))

move :: XO -> IO Loc
move xo = do
  putStrLn $ "Turn: " ++ show xo
  str <- getLine
  case readMay str of
    Nothing -> do
      putStrLn "Try again."
      move xo
    Just (x,y) ->
        return $ Loc x y

forfeit :: Win XO -> Lose XO -> IO ()
forfeit (Win w) (Lose l) = do
  putStrLn $ show l ++ " forfeited."
  putStrLn $ show w ++ " won."

end :: Win XO -> Lose XO -> IO ()
end (Win w) (Lose l) = do
  putStrLn $ show w ++ " won, and " ++ show l ++ " lost."

tie :: IO ()
tie = putStrLn "Tie game!"

step :: Board -> XO -> Loc -> IO ()
step b p loc = do
  let d = boardList b
  let cell :: Maybe XO -> String
      cell mcell = maybe " " show mcell
  putStrLn $ show p ++ " moved to " ++ show loc
  putStrLn $ cell (d !! 0) ++ "|" ++  cell (d !! 1) ++ "|" ++ cell (d !! 2)
  putStrLn "-+-+-"
  putStrLn $ cell (d !! 3) ++ "|" ++  cell (d !! 4) ++ "|" ++ cell (d !! 5)
  putStrLn "-+-+-"
  putStrLn $ cell (d !! 6) ++ "|" ++  cell (d !! 7) ++ "|" ++ cell (d !! 8)
