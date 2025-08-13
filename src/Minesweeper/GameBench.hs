
module Minesweeper.GameBench (run) where

import Control.Monad.ST
import System.Random.Stateful

import Helpers.TimeIt (myTimeIt)

import Minesweeper.Game

run :: IO ()
run = do
  gen <- getStdGen
  let g = runST (runStateGenT gen (mkGame (16, 30, 99)))
  myTimeIt "Minesweeper, mkGame" g
  myTimeIt "Minesweeper, play" $ play (MoveFree 0 0) $ fst g    -- doesn't recompute g

