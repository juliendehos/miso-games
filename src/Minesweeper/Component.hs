
module Minesweeper.Component where

-- import Control.Monad.Primitive
-- import Data.Functor.Identity
import Control.Monad.ST
import Miso
import System.Random

import Minesweeper.Model
import Minesweeper.Update
import Minesweeper.View

{-

mkComponent :: StdGen -> App Model Action
-- mkComponent :: StdGen -> Component m Model Action
mkComponent gen = 
  let initialModel = runST $ mkModel ModeBeginner gen
  in (component initialModel updateModel viewModel) 
    { events = defaultEvents <> pointerEvents
    }

-}

mkComponent :: Component m Model Action
mkComponent = do
  let gen = mkStdGen 42
  let initialModel = runST $ mkModel ModeBeginner gen
  (component initialModel updateModel viewModel)
    { events = defaultEvents <> pointerEvents
    }

