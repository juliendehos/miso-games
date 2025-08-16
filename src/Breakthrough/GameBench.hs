
module Breakthrough.GameBench (mkGroups) where

import Criterion.Main

import Game
import Breakthrough.Game

mkGroups :: IO [Benchmark]
mkGroups = do
  pure 
    [ bgroup "Breakthrough.Game" 
        [ bench "mkGame" $ 
            whnf (mkGame 8) 8
        , bench "reset" $ 
            whnf reset (mkGame 8 8)
        , bench "play" $ 
            whnf (play (Move (6, 1) (5, 1))) (mkGame 8 8)
        ]
    ]

