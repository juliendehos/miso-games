
module Minesweeper.Component where

import Control.Monad.ST
import Miso
import System.Random

import Minesweeper.Model
import Minesweeper.Update
import Minesweeper.View

mkComponent :: StdGen -> Component m Model Action
mkComponent gen = do
  -- let gen = mkStdGen 42
  let initialModel = runST $ mkModel ModeBeginner gen
  (component initialModel updateModel viewModel)
    { events = defaultEvents <> pointerEvents
    }

