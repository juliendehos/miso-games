
module Bot.MonteCarloBench (mkGroups) where

import Control.Monad.State.Strict
import Criterion.Main
import System.Random

import Breakthrough.Game
import Bot.MonteCarlo

mkGroups :: IO [Benchmark]
mkGroups = do
  gen <- getStdGen
  let game = Breakthrough.Game.mkGame 8 8
  pure 
    [ bgroup "MonteCarlo Breakthrough" 
        [ bench "genMove' 2" $ 
            nf (Bot.MonteCarlo.genMove' 2 game) gen
        , bench "genMove 2" $ 
            whnf (runState (Bot.MonteCarlo.genMove 2 game)) gen
        , bench "genMove' 5" $ 
            nf (Bot.MonteCarlo.genMove' 5 game) gen
        , bench "genMove 5" $ 
            whnf (runState (Bot.MonteCarlo.genMove 5 game)) gen
        ]
    ]

