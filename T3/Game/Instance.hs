{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module T3.Game.Instance where

import Prelude
import Control.Monad.Trans
import Safe
import T3.Game

-- Try it in the REPL

newtype Terminal a = Terminal { unTerminal :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance Game Terminal where
  move xo = do
    liftIO $ putStrLn $ "Turn: " ++ show xo
    str <- liftIO $ getLine
    case readMay str of
      Nothing -> do
        liftIO $ putStrLn "Try again."
        move xo
      Just (x,y) ->
        return $ Loc x y
  forfeit (Win w) (Lose l) = do
    liftIO $ putStrLn $ show l ++ " forfeited."
    liftIO $ putStrLn $ show w ++ " won."
  end (Win w) (Lose l) = do
    liftIO $ putStrLn $ show w ++ " won, and " ++ show l ++ " lost."
  tie = liftIO $ putStrLn "Tie game!"
  step b = do
    let d = boardList b
    let cell :: Maybe XO -> String
        cell mcell = maybe " " show mcell
    liftIO $ putStrLn $ cell (d !! 0) ++ "|" ++  cell (d !! 1) ++ "|" ++ cell (d !! 2)
    liftIO $ putStrLn "-+-+-"
    liftIO $ putStrLn $ cell (d !! 3) ++ "|" ++  cell (d !! 4) ++ "|" ++ cell (d !! 5)
    liftIO $ putStrLn "-+-+-"
    liftIO $ putStrLn $ cell (d !! 6) ++ "|" ++  cell (d !! 7) ++ "|" ++ cell (d !! 8)
