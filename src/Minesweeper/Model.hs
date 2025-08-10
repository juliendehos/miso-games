{-# LANGUAGE OverloadedStrings #-}

module Minesweeper.Model where

import Control.Monad.Primitive
import Miso.Lens
import Miso.Lens.TH
import Miso.String (MisoString)
import System.Random.Stateful

import Minesweeper.Game

data Mode
  = ModeBeginner
  | ModeIntermediate
  | ModeExpert

mode2infos :: Mode -> (Int, Int, Int)
mode2infos = \case
  ModeBeginner      -> (9, 9, 10)
  ModeIntermediate  -> (16, 16, 40)
  ModeExpert        -> (16, 30, 99)

data Model = Model
  { _modelLog   :: MisoString
  , _modelGame  :: Game
  , _modelGen   :: StdGen
  } deriving (Eq)

makeLenses ''Model

mkModel :: (PrimMonad m) => Mode -> StdGen -> m Model
mkModel mode gen = 
  uncurry (Model "this is Minesweeper") <$> runStateGenT gen (mkGame $ mode2infos mode)

resetModel :: (PrimMonad m) => Mode -> Model -> m Model
resetModel mode model = 
  (modelLog .~ "new game") <$> mkModel mode (model ^. modelGen)

