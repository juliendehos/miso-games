
module Bot.MonteCarloBench (run) where

import Control.Monad.State.Strict
import System.Random

import Bot.MonteCarlo
import Breakthrough.Game
import Helpers.TimeIt (myTimeIt)

run :: IO ()
run = do

  let game = Breakthrough.Game.mkGame 8 8
  myTimeIt "MonteCarlo, Breakthrough, mkGame 8 8" game

  gen <- getStdGen
  myTimeIt "MonteCarlo, Breakthrough, genMove 10" $
    runState (Bot.MonteCarlo.genMove 10 game) gen

  myTimeIt "MonteCarlo, Breakthrough, genMove 20" $
    runState (Bot.MonteCarlo.genMove 20 game) gen

