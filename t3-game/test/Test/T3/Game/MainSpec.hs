module Test.T3.Game.MainSpec (spec) where

import Prelude hiding (log)
import Control.Monad.Trans.Class (lift)
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Test.Hspec

import T3.Core (XO(X,O))

import T3.Game.Main (main)
import T3.Game.Play (Play)

mkFixture "Fixture" [''Play]

spec :: Spec
spec = do
  describe "main" $ do
    it "should pass X as the first player and O as the second player" $ do
      calls <- logTestFixtureT main $ def
        { _play = \p0 p1 -> do
            log "play"
            lift $ p0 `shouldBe` X
            lift $ p1 `shouldBe` O
        }
      calls `shouldBe` ["play"]
