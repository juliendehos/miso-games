{-# LANGUAGE OverloadedStrings #-}

module Tictactoe.Model where

import Control.Monad (when)
import Miso
import Miso.Lens
import Miso.Lens.TH
import Miso.Canvas (Canvas, canvas)
import Miso.Canvas qualified as Canvas 
import Miso.Style qualified as Style

import Tictactoe.Game

data Model = Model
  { _modelGame    :: Game
  , _modelLog     :: MisoString
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model mkGame "this is Tictactoe" 

