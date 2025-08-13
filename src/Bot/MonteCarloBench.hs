{-# LANGUAGE Strict #-}

module Bot.MonteCarloBench (run) where

import Control.Monad
import Control.Monad.State.Strict
import System.Random
import Text.Printf

import Bot.MonteCarlo
import Breakthrough.Game
import Helpers.TimeIt

run :: IO ()
run = do

  gen <- getStdGen
  let game = Breakthrough.Game.mkGame 8 8
  myTimeIt "MonteCarlo, Breakthrough, mkGame 8 8" game

{-
  myTimeIt "MonteCarlo, Breakthrough, genMove 10" $
    Bot.MonteCarlo.genMove' 10 game gen
    -- runState (Bot.MonteCarlo.genMove 10 game) gen
-}

  myTimeIt "MonteCarlo, Breakthrough, genMove 20" $
    runState (Bot.MonteCarlo.genMove 20 game) gen

  -- timeItNamed "mc, genMove', 20" $ Bot.MonteCarlo.genMove' 20 game gen `seq` pure ()

  (t, _) <- timeItT $ let y = Bot.MonteCarlo.genMove' 20 game gen in y `seq` void (print y)
  printf "mc 20: %6.6fs\n" t

  pure ()


