
module Breakthrough.GameBench (run) where

import Helpers.TimeIt (myTimeIt)

import Game
import Breakthrough.Game

run :: IO ()
run = do
  let g = mkGame 8 8
  myTimeIt "Breakthrough, mkGame" g
  myTimeIt "Breakthrough, reset" $ reset g    -- doesn't recompute g
  myTimeIt "Breakthrough, play" $ play (Move (1, 1) (2, 1)) g

