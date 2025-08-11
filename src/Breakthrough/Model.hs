{-# LANGUAGE OverloadedStrings #-}

module Breakthrough.Model where

import Miso
import Miso.Lens
import Miso.Lens.TH
import System.Random

import Breakthrough.Game

data Model = Model
  { _modelGame      :: Game
  , _modelSelected  :: Maybe (Int, Int)
  , _modelLog       :: MisoString
  } deriving (Eq)
  -- TODO _modelGen, _modelBotType

makeLenses ''Model

mkModel :: StdGen -> Model
mkModel _gen = Model (mkGame 8 8) Nothing "this is Breakthrough"    -- TODO gen BotType

