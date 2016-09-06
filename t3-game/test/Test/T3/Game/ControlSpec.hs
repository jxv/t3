module Test.T3.Game.ControlSpec (spec) where

import Prelude hiding (log)
import Control.Monad.Trans.Class (lift)
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Test.Hspec

import T3.Core (XO(X,O), Loc(..), Result(Unfinished,Tie,Winner))

import T3.Game.Control (move, forfeit, end, tie)

import T3.Game.Types (Win(Win), Lose(Lose), Final(..))
import T3.Game.Classes (Communicator)

mkFixture "Fixture" [''Communicator]

pop :: Monad m => TestFixtureT Fixture [log] [expected] m expected
pop = do
  x:xs <- get
  put xs
  return x

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

  describe "forfeit" $ do
    it "should send the final by DQ to the winner then loser" $ do
      let (stubW, stubL) = (X, O)

      (_, calls) <- execTestFixtureT (forfeit (Win stubW) (Lose stubL)) def
        { _sendFinal = \xo final -> do
            log "sendFinal"
            expected <- pop
            lift $ (xo, final) `shouldBe` expected
        }
        [(stubW, WonByDQ), (stubL, LossByDQ)]

      calls `shouldBe` ["sendFinal", "sendFinal"]

  describe "end" $ do
    it "should send the final the winner then loser" $ do
      let (stubW, stubL) = (X, O)

      (_, calls) <- execTestFixtureT (end (Win stubW) (Lose stubL)) def
        { _sendFinal = \xo final -> do
            log "sendFinal"
            expected <- pop
            lift $ (xo, final) `shouldBe` expected
        }
        [(stubW, Won), (stubL, Loss)]

      calls `shouldBe` ["sendFinal", "sendFinal"]

  describe "tie" $ do
    it "should send the tied final to X then O" $ do
      (_, calls) <- execTestFixtureT tie def
        { _sendFinal = \xo final -> do
            log "sendFinal"
            expected <- pop
            lift $ (xo, final) `shouldBe` expected
        }
        [(X, Tied), (O, Tied)]

      calls `shouldBe` ["sendFinal", "sendFinal"]
