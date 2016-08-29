module Test.T3.Game.ControlSpec (spec) where

import Prelude hiding (log)
import Control.Monad.Trans.Class (lift)
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Test.Hspec

import T3.Core (XO(X,O), Loc(..), Result(Unfinished,Tie,Winner))

import T3.Game.Types (Win(Win), Lose(Lose))
import T3.Game.ControlImpl (move, forfeit, end, tie)
import T3.Game.Parts (Communicator)

mkFixture "Fixture" [''Communicator]

spec :: Spec
spec = do
  describe "move" $ do
      it "should send the game state then receive an location" $ do
        let stubXO = X
        let stubLoc = Loc { _locX = 1, _locY = 1 }

        (loc, calls) <- evalTestFixtureT (move stubXO) def
          { _sendGameState = \xo -> do
              log "sendGameState"
              lift $ xo `shouldBe` stubXO
          , _recvAction = \xo -> do
              log "recvAction"
              lift $ xo `shouldBe` stubXO
              return stubLoc
          }

        calls `shouldBe` ["sendGameState", "recvAction"]
        loc `shouldBe` stubLoc
