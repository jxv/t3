module T3.GameConsole.BoardManager
  ( insertAtLoc'
  , isOpenLoc'
  , getResult'
  ) where

import Prelude hiding (putStrLn)

import qualified T3.Game.BoardManager as Game
import T3.Core (boardList, Loc, XO, Result)
import T3.Game.HasBoard (HasBoard(getBoard))
import T3.GameConsole.Console (Console(putStrLn))

insertAtLoc' :: (HasBoard m, Console m) => Loc -> XO -> m ()
insertAtLoc' loc xo = do
  Game.insertAtLoc' loc xo
  printChange loc xo

isOpenLoc' :: HasBoard m => Loc -> m Bool
isOpenLoc' = Game.isOpenLoc'

getResult' :: HasBoard m => m Result
getResult' = Game.getResult'

printChange :: (HasBoard m, Console m) => Loc -> XO -> m ()
printChange loc xo = do
  [c1,c2,c3,c4,c5,c6,c7,c8,c9] <- boardList <$> getBoard
  putStrLn (moveStatement xo loc)
  putStrLn (rowCells c1 c2 c3)
  putStrLn rowBorder
  putStrLn (rowCells c4 c5 c6)
  putStrLn rowBorder
  putStrLn (rowCells c7 c8 c9)

moveStatement :: XO -> Loc -> String
moveStatement xo loc = show xo ++ " moved to " ++ show loc

rowBorder :: String
rowBorder = "-+-+-"

rowCells :: Maybe XO -> Maybe XO -> Maybe XO -> String
rowCells a b c = cell a ++ "|" ++ cell b ++ "|" ++ cell c
  where
    cell mcell = maybe " " show mcell
