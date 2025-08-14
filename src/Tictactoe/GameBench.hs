{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tictactoe.GameBench (run) where

import Control.Monad

import Game
import Helpers.TimeIt
import Tictactoe.Game

instance Show Tictactoe.Game.Game where
  show _ = "Tictactoe.Game"

run :: IO ()
run = do

  (_, game) <- myTimeIt "Tictactoe, mkGame" mkGame

  void $ myTimeIt "Tictactoe, reset" $ reset game

  void $ myTimeIt "Tictactoe, play" $ play (Move 1 1) game

