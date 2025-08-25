{-# LANGUAGE OverloadedStrings #-}

module App.Model where

import Control.Monad.ST
import Miso.Lens
import Miso.Lens.TH
import System.Random

import Breakthrough.Model as Breakthrough
import ConnectFour.Model as ConnectFour
import Minesweeper.Model as Minesweeper
import Tictactoe.Model as Tictactoe

data GameType
  = Breakthrough
  | Breakthrough86
  | ConnectFour
  | Minesweeper
  | Tictactoe
  deriving (Eq)

data AppModel = AppModel
  { _modelGameType        :: GameType
  , _modelBreakthrough    :: Breakthrough.Model
  , _modelBreakthrough86  :: Breakthrough.Model
  , _modelConnectFour     :: ConnectFour.Model
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
    connectFourModel = ConnectFour.mkModel gen3'

    (gen4, gen4') = splitGen gen3
    minesweeperModel = runST $ Minesweeper.mkModel Minesweeper.ModeBeginner gen4'

    gen5' = gen4
    tictactoeModel = Tictactoe.mkModel gen5'

  in AppModel
      Tictactoe
      breakthroughModel
      breakthroughModel86
      connectFourModel
      minesweeperModel
      tictactoeModel

