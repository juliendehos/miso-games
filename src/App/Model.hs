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
  | Breakthrough86
  | Minesweeper
  | Tictactoe
  deriving (Eq)

data AppModel = AppModel
  { _modelGameType        :: GameType
  , _modelBreakthrough    :: Breakthrough.Model
  , _modelBreakthrough86  :: Breakthrough.Model
  , _modelMinesweeper     :: Minesweeper.Model
  , _modelTictactoe       :: Tictactoe.Model
  } deriving (Eq)

makeLenses ''AppModel

mkAppModel :: StdGen -> AppModel
mkAppModel gen0 = 
  let
    (gen1, gen1') = splitGen gen0
    breakthroughModel = Breakthrough.mkModel 8 8 gen1'

    (gen2, gen2') = splitGen gen1
    breakthroughModel86 = Breakthrough.mkModel 8 6 gen2'

    (gen3, gen3') = splitGen gen2
    minesweeperModel = runST $ Minesweeper.mkModel Minesweeper.ModeBeginner gen3'

    gen4' = gen3
    tictactoeModel = Tictactoe.mkModel gen4'

  in AppModel
      Tictactoe
      breakthroughModel
      breakthroughModel86
      minesweeperModel
      tictactoeModel

