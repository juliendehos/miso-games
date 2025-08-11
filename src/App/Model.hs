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
mkAppModel gen0 = 
  let
    (gen1, gen1') = splitGen gen0
    breakthroughModel = Breakthrough.mkModel gen1'

    (gen2, gen2') = splitGen gen1
    minesweeperModel = runST $ Minesweeper.mkModel Minesweeper.ModeBeginner gen2'

    gen3' = gen2
    tictactoeModel = Tictactoe.mkModel gen3'

  in AppModel
      Breakthrough
      breakthroughModel
      minesweeperModel
      tictactoeModel

