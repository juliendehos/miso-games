
module Tictactoe.GameBench (groups) where

import Criterion.Main

import Game
import Tictactoe.Game

groups :: [Benchmark]
groups = 
  [ bgroup "Tictactoe.Game" 
      [ bench "reset" $ whnf reset mkGame
      , bench "play" $ whnf (play (Move 1 1)) mkGame
      ]
  ]

