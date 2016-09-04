module T3.Game.Main
  ( main
  ) where

import T3.Core (XO(X,O))
import T3.Game.Classes (Play(play))

main :: Play m => m ()
main = play X O
