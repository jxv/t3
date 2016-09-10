module Test.T3.Game.PlaySpec (spec) where

import Prelude hiding (log)
import Control.Monad.Trans.Class (lift)
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Test.Hspec

import T3.Core (XO(X,O), Loc(..), Result(Unfinished,Tie,Winner))

import T3.Game.Play (play')

import T3.Game.Types (Win(Win), Lose(Lose))
import T3.Game.Play (Play)
import T3.Game.BoardManager (BoardManager)
import T3.Game.Control (Control)

mkFixture "Fixture" [''Play, ''BoardManager, ''Control]

spec :: Spec
spec = do
  describe "play'" $ do
    it "should forfeit given an non-opened location as a move" $ do
      let (p0, p1) = (X, O)
      let stubLoc = Loc { _locX = 0, _locY = 0 }

      calls <- logTestFixtureT (play' p0 p1) $ def
        { _move = \p -> do
            log "move"
            lift $ p `shouldBe` p0
            return stubLoc
        , _isOpenLoc = \loc -> do
            log "isOpenLoc"
            lift $ loc `shouldBe` stubLoc
            return False
        , _forfeit = \(Win w) (Lose l) -> do
            log "forfeit"
            lift $ w `shouldBe` p1
            lift $ l `shouldBe` p0
        }

      calls `shouldBe` ["move", "isOpenLoc", "forfeit"]

    it "should insert the move, get the result, and switch turns" $ do
      let (p0, p1) = (X, O)
      let stubLoc = Loc { _locX = 0, _locY = 0 }

      calls <- logTestFixtureT (play' p0 p1) $ def
        { _move = \p -> do
            log "move"
            lift $ p `shouldBe` p0
            return stubLoc
        , _isOpenLoc = \loc -> do
            log "isOpenLoc"
            lift $ loc `shouldBe` stubLoc
            return True
        , _insertAtLoc = \loc p -> do
            log "insertAtLoc"
            lift $ loc `shouldBe` stubLoc
            lift $ p `shouldBe` p0
        , _getResult = do
            log "getResult"
            return Unfinished
        , _play = \q0 q1 -> do
            log "play"
            lift $ q0 `shouldBe` p1
            lift $ q1 `shouldBe` p0
        }

      calls `shouldBe` ["move", "isOpenLoc", "insertAtLoc", "getResult", "play"]

    it "should insert the move, get the result, and tie" $ do
      let (p0, p1) = (X, O)
      let stubLoc = Loc { _locX = 0, _locY = 0 }

      calls <- logTestFixtureT (play' p0 p1) $ def
        { _move = \p -> do
            log "move"
            lift $ p `shouldBe` p0
            return stubLoc
        , _isOpenLoc = \loc -> do
            log "isOpenLoc"
            lift $ loc `shouldBe` stubLoc
            return True
        , _insertAtLoc = \loc p -> do
            log "insertAtLoc"
            lift $ loc `shouldBe` stubLoc
            lift $ p `shouldBe` p0
        , _getResult = do
            log "getResult"
            return Tie
        , _tie = do
            log "tie"
        }

      calls `shouldBe` ["move", "isOpenLoc", "insertAtLoc", "getResult", "tie"]

    it "should insert the move, get the result, and end" $ do
      let (p0, p1) = (X, O)
      let stubLoc = Loc { _locX = 0, _locY = 0 }

      calls <- logTestFixtureT (play' p0 p1) $ def
        { _move = \p -> do
            log "move"
            lift $ p `shouldBe` p0
            return stubLoc
        , _isOpenLoc = \loc -> do
            log "isOpenLoc"
            lift $ loc `shouldBe` stubLoc
            return True
        , _insertAtLoc = \loc p -> do
            log "insertAtLoc"
            lift $ loc `shouldBe` stubLoc
            lift $ p `shouldBe` p0
        , _getResult = do
            log "getResult"
            return (Winner (error "winner"))
        , _end = \(Win w) (Lose l) -> do
            log "end"
            lift $ w `shouldBe` p0
            lift $ l `shouldBe` p1
        }

      calls `shouldBe` ["move", "isOpenLoc", "insertAtLoc", "getResult", "end"]
