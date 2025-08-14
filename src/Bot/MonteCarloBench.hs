{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bot.MonteCarloBench (run) where

import Control.Monad
import Control.Monad.State.Strict
import System.Random

import Bot.MonteCarlo
import Breakthrough.Game
import Helpers.TimeIt

instance Show Breakthrough.Game.Game where
  show _ = "Breakthrough.Game"

run :: IO ()
run = do

  gen <- getStdGen

  (_, game) <- myTimeIt "MonteCarlo, Breakthrough, mkGame" $
    Breakthrough.Game.mkGame 8 8

  void $ myTimeIt "MonteCarlo, Breakthrough, genMove' 5" $
    Bot.MonteCarlo.genMove' 5 game gen

  void $ myTimeIt "MonteCarlo, Breakthrough, genMove 5" $
    runState (Bot.MonteCarlo.genMove 5 game) gen

  void $ myTimeIt "MonteCarlo, Breakthrough, genMove' 10" $
    Bot.MonteCarlo.genMove' 10 game gen

  void $ myTimeIt "MonteCarlo, Breakthrough, genMove 10" $
    runState (Bot.MonteCarlo.genMove 10 game) gen

