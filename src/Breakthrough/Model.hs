{-# LANGUAGE OverloadedStrings #-}

module Breakthrough.Model where

import Miso
import Miso.Lens
import Miso.Lens.TH

import Breakthrough.Game

data Model = Model
  { _modelGame      :: Game
  , _modelSelected  :: Maybe (Int, Int)
  , _modelLog       :: MisoString
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model (mkGame 8 8) Nothing "this is Breakthrough" 

