{-# OPTIONS_GHC -fno-warn-orphans #-}

module Breakthrough.GameBench (run) where

import Control.Monad

import Breakthrough.Game
import Game
import Helpers.TimeIt (myTimeIt)

instance Show Breakthrough.Game.Game where
  show _ = "Breakthrough.Game"

run :: IO ()
run = do

  (_, game) <- myTimeIt "Breakthrough, mkGame" $ mkGame 8 8

  void $ myTimeIt "Breakthrough, reset" $ reset game

  void $ myTimeIt "Breakthrough, play" $ play (Move (6, 1) (5, 1)) game

