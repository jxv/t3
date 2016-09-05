module Main where

import qualified T3.Game.Main as Game (main)
import qualified T3.GameConsole.Monad as GameConsole (runIO)
import T3.Core (emptyBoard)

main :: IO ()
main = GameConsole.runIO Game.main emptyBoard
