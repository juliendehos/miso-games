
module Tictactoe.GameBench (run) where

import Helpers.TimeIt (myTimeIt)

import Game
import Tictactoe.Game

run :: IO ()
run = do
  let g = mkGame
  myTimeIt "Tictactoe, mkGame" g
  myTimeIt "Tictactoe, reset" $ reset g   -- doesn't recompute g
  myTimeIt "Tictactoe, play" $ play (Move 1 1) g

