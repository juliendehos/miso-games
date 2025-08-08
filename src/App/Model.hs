
module App.Model where

import Control.Monad.Primitive
import Miso.Lens
import Miso.Lens.TH
import System.Random

data GameType
  = Breakthrough
  | Minesweeper
  | Tictactoe
  deriving (Eq)

data Model = Model
  { _mGameType :: GameType
  , _mGen :: StdGen
  } deriving (Eq)

makeLenses ''Model

mkModel :: (PrimMonad m) => StdGen -> m Model
mkModel = pure . Model Minesweeper

