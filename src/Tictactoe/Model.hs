{-# LANGUAGE OverloadedStrings #-}

module Tictactoe.Model where

import Miso
import Miso.Lens
import Miso.Lens.TH

import Tictactoe.Game

data Model = Model
  { _modelGame    :: Game
  , _modelLog     :: MisoString
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model mkGame "this is Tictactoe" 

