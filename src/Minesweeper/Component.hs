
module Minesweeper.Component where

-- import Control.Monad.Primitive
-- import Data.Functor.Identity
import Control.Monad.ST
import Miso
import System.Random.Stateful

import Minesweeper.Model
import Minesweeper.Helpers
import Minesweeper.Update
import Minesweeper.View

mkComponent :: StdGen -> App Model Action
-- mkComponent :: StdGen -> Component m Model Action
mkComponent gen = 
  let initialModel = runST $ mkModel ModeBeginner gen
  in (component initialModel updateModel viewModel) 
    { events = defaultEvents <> pointerEvents
    }


