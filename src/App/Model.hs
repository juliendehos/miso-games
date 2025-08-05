
module App.Model where

import Control.Monad.Primitive
import Miso.Lens
import Miso.Lens.TH
import System.Random.Stateful

data Model = Model
  { _mGen :: StdGen   -- TODO TVar StdGen ?
  } deriving (Eq)

makeLenses ''Model

mkModel :: (PrimMonad m) => StdGen -> m Model
mkModel = pure . Model


