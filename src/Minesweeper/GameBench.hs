
module Minesweeper.GameBench (mkGroups) where

import Control.Monad.ST
import Criterion.Main
import System.Random.Stateful

import Minesweeper.Game

mkGroups :: IO [Benchmark]
mkGroups = do
  gen <- getStdGen
  (game, _) <- runStateGenT gen (mkGame (16, 30, 99))
  pure 
    [ bgroup "Minesweeper" 
        [ bench "mkGame 16 30 99" $ 
            whnfIO (runStateGenT gen (mkGame (16, 30, 99)))
        , bench "mkGame (runST gen)" $ 
            whnf (\g -> runST (runStateGenT g (mkGame (16, 30, 99)))) gen
        , bench "mkGame (runST params)" $ 
            whnf (\params -> runST (runStateGenT gen (mkGame params))) (16, 30, 99)
        , bench "play (MoveFree 0 0)" $ 
            whnf (play (MoveFree 0 0)) game
        ]
    ]

