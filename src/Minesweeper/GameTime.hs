{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minesweeper.GameTime (run) where

import Control.Monad
import Control.Monad.ST
import System.Random.Stateful

import Helpers.TimeIt (myTimeIt)
import Minesweeper.Game

instance Show Minesweeper.Game.Game where
  show _ = "Minesweeper.Game"

run :: IO ()
run = do

  gen <- getStdGen

  (_, (game, _)) <- myTimeIt "Minesweeper, mkGame" $ 
    runST (runStateGenT gen (mkGame (16, 30, 99)))

  void $ myTimeIt "Minesweeper, play" $ 
    play (MoveFree 0 0) game

