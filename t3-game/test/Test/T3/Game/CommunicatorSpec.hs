module Test.T3.Game.CommunicatorSpec (spec) where

import Prelude hiding (log)
import Control.Monad.Trans.Class (lift)
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Test.Hspec

import T3.Core (XO(X,O), Loc(..), emptyBoard)

import T3.Game.CommunicatorImpl (sendGameState, recvAction, sendFinal)
import T3.Game.Types (Final(..), Step(..))
import T3.Game.Classes (HasBoard, Transmitter)

mkFixture "Fixture" [''HasBoard, ''Transmitter]

spec :: Spec
spec = do
  describe "sendGameState" $ do
    it "should send a step" $ do
      let stubXO = X
      let stubBoard = emptyBoard

      calls <- logTestFixtureT (sendGameState stubXO) def
        { _getBoard = do
            log "getBoard"
            return emptyBoard
        , _sendStep = \xo step -> do
            log "sendStep"
            lift $ xo `shouldBe` stubXO
            lift $ step `shouldBe` Step{ _stepBoard = stubBoard, _stepFinal = Nothing }
        }

      calls `shouldBe` ["getBoard", "sendStep"]

  describe "sendFinal" $ do
    it "should send a step with a final" $ do
      let stubXO = X
      let stubBoard = emptyBoard
      let stubFinal = Won

      calls <- logTestFixtureT (sendFinal stubXO stubFinal) def
        { _getBoard = do
            log "getBoard"
            return emptyBoard
        , _sendStep = \xo step -> do
            log "sendStep"
            lift $ xo `shouldBe` stubXO
            lift $ step `shouldBe` Step{ _stepBoard = stubBoard, _stepFinal = Just stubFinal }
        }

      calls `shouldBe` ["getBoard", "sendStep"]

  describe "recvAction" $ do
    it "should be the same as recvLoc" $ do
      let stubXO = X
      let stubLoc = Loc { _locX = 10, _locY = 10 }

      (actual, calls) <- evalTestFixtureT (recvAction stubXO) def
        { _recvLoc = \xo -> do
            log "recvLoc"
            lift $ xo `shouldBe` stubXO
            return stubLoc
        }

      actual `shouldBe` stubLoc
      calls `shouldBe` ["recvLoc"]
