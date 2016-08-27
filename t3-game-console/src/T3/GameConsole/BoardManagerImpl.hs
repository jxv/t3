module T3.GameConsole.BoardManagerImpl
  ( insertAtLoc
  ) where

import Prelude hiding (putStrLn)

import qualified T3.Game.BoardManagerImpl as BoardManager
import T3.Core (boardList, Loc, XO)
import T3.Game.Parts (HasBoard(getBoard))

import T3.GameConsole.Parts (Console(putStrLn))

moveStatement :: XO -> Loc -> String
moveStatement xo loc = show xo ++ " moved to " ++ show loc

rowBorder :: String
rowBorder = "-+-+-"

rowCells :: Maybe XO -> Maybe XO -> Maybe XO -> String
rowCells a b c = cell a ++ "|" ++ cell b ++ "|" ++ cell c
  where
    cell mcell = maybe " " show mcell

insertAtLoc :: (HasBoard m, Console m) => Loc -> XO -> m ()
insertAtLoc loc xo = do
  BoardManager.insertAtLoc loc xo
  [c1,c2,c3,c4,c5,c6,c7,c8,c9] <- boardList <$> getBoard
  putStrLn (moveStatement xo loc)
  putStrLn (rowCells c1 c2 c3)
  putStrLn rowBorder
  putStrLn (rowCells c4 c5 c6)
  putStrLn rowBorder
  putStrLn (rowCells c7 c8 c9)
