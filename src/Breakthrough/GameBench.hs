
module Breakthrough.GameBench (run) where

import Breakthrough.Game
import Game
import Helpers.TimeIt (myTimeIt)

run :: IO ()
run = do
  let g = mkGame 8 8
  myTimeIt "Breakthrough, mkGame" g
  myTimeIt "Breakthrough, reset" $ reset g    -- doesn't recompute g
  myTimeIt "Breakthrough, play" $ play (Move (1, 1) (2, 1)) g

