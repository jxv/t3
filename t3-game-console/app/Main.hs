module Main where

import qualified T3.Game.Main as Game (main)
import T3.GameConsole.Monad (runGameConsole)
import T3.Core (emptyBoard)

main :: IO ()
main = runGameConsole Game.main emptyBoard
