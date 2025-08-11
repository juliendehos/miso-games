{-# LANGUAGE OverloadedStrings #-}

module App.Model where

import Control.Monad.ST
import Miso.Lens
import Miso.Lens.TH
import System.Random

import Breakthrough.Model as Breakthrough
import Minesweeper.Model as Minesweeper
import Tictactoe.Model as Tictactoe

data GameType
  = Breakthrough
  | Minesweeper
  | Tictactoe
  deriving (Eq)

data AppModel = AppModel
  { _modelGameType      :: GameType
  , _modelBreakthrough  :: Breakthrough.Model
  , _modelMinesweeper   :: Minesweeper.Model
  , _modelTictactoe     :: Tictactoe.Model
  } deriving (Eq)

makeLenses ''AppModel

mkAppModel :: StdGen -> AppModel
mkAppModel gen = 
  AppModel
    Breakthrough
    Breakthrough.mkModel
    (runST $ Minesweeper.mkModel Minesweeper.ModeBeginner gen)
    Tictactoe.mkModel

