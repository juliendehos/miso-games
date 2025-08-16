
module Tictactoe.GameBench (mkGroups) where

import Criterion.Main

import Game
import Tictactoe.Game

mkGroups :: IO [Benchmark]
mkGroups = do
  pure 
    [ bgroup "Tictactoe.Game" 
        [ bench "reset" $ 
            whnf reset mkGame
        , bench "play" $ 
            whnf (play (Move 1 1)) mkGame
        ]
    ]

