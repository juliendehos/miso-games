
module Breakthrough.Game where

import Data.Vector as V hiding (replicate)
import Miso
import Miso.Lens
import Miso.Lens.TH

import Helpers.Board

data Move = Move
  { _moveFrom :: (Int, Int)
  , _moveTo   :: (Int, Int)
  } deriving (Eq)

makeLenses ''Move

data Status
  = RedPlays
  | BluePlays
  | RedWins
  | BlueWins

data Cell
  = CellEmpty
  | CellRed
  | CellBlue

data Game = Game
  { _gameBoard  :: Board Cell
  , _gameStatus :: Status
  }

makeLenses ''Game

mkGame :: Game
mkGame = Game board RedPlays
  where
    board = mkBoard 8 8 (replicate 64 CellEmpty)

