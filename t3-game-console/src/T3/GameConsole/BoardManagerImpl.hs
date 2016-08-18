module T3.GameConsole.BoardManagerImpl
  ( insertAtLoc
  ) where

import Prelude hiding (putStrLn)

import qualified T3.Game.BoardManagerImpl as BoardManager
import T3.Core (boardList, Loc, XO)
import T3.Game.HasBoard (HasBoard(getBoard))

import T3.GameConsole.Console (Console(putStrLn))

insertAtLoc :: (HasBoard m, Console m) => Loc -> XO -> m ()
insertAtLoc loc xo = do
  BoardManager.insertAtLoc loc xo
  board <- getBoard
  let d = boardList board
  let cell mcell = maybe " " show mcell
  putStrLn $ show xo ++ " moved to " ++ show loc
  putStrLn $ cell (d !! 0) ++ "|" ++  cell (d !! 1) ++ "|" ++ cell (d !! 2)
  putStrLn "-+-+-"
  putStrLn $ cell (d !! 3) ++ "|" ++  cell (d !! 4) ++ "|" ++ cell (d !! 5)
  putStrLn "-+-+-"
  putStrLn $ cell (d !! 6) ++ "|" ++  cell (d !! 7) ++ "|" ++ cell (d !! 8)
