{-# OPTIONS_GHC -fno-warn-orphans #-}
module T3.Game.Instance where

import Prelude
import Safe
import T3.Game

-- Try it in the REPL

instance Game IO where
  move xo = do
    putStrLn $ "Turn: " ++ show xo
    str <- getLine
    case readMay str of
      Nothing -> do
        putStrLn "Try again."
        move xo
      Just (x,y) ->
        return $ Loc x y
  forfeit (Win w) (Lose l) = do
    putStrLn $ show l ++ " forfeited."
    putStrLn $ show w ++ " won."
  end (Win w) (Lose l) = do
    putStrLn $ show w ++ " won, and " ++ show l ++ " lost."
  tie = putStrLn "Tie game!"
  step b = do
    let d = boardList b
    let cell :: Maybe XO -> String
        cell mcell = maybe " " show mcell
    putStrLn $ cell (d !! 0) ++ "|" ++  cell (d !! 1) ++ "|" ++ cell (d !! 2)
    putStrLn "-+-+-"
    putStrLn $ cell (d !! 3) ++ "|" ++  cell (d !! 4) ++ "|" ++ cell (d !! 5)
    putStrLn "-+-+-"
    putStrLn $ cell (d !! 6) ++ "|" ++  cell (d !! 7) ++ "|" ++ cell (d !! 8)
