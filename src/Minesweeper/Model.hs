
module Minesweeper.Model where

import Control.Monad.Primitive
import Miso.Lens
import Miso.Lens.TH
import System.Random.Stateful

import Minesweeper.Game

data Model = Model
  { _mGame :: Game
  , _mGen :: StdGen
  } deriving (Eq)

makeLenses ''Model

mkModel :: (PrimMonad m) => Mode -> StdGen -> m Model
mkModel mode gen0 = uncurry Model <$> runStateGenT gen0 (mkGame $ mode2infos mode)

resetModel :: (PrimMonad m) => Mode -> Model -> m Model
resetModel mode = mkModel mode . _mGen

data Mode
  = ModeBeginner
  | ModeIntermediate
  | ModeExpert

mode2infos :: Mode -> (Int, Int, Int)
mode2infos = \case
  ModeBeginner      -> (9, 9, 10)
  ModeIntermediate  -> (16, 16, 40)
  ModeExpert        -> (16, 30, 99)

