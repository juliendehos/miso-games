
import Control.Monad.IO.Class (liftIO)
import Miso
import System.Random (getStdGen)

import Minesweeper.Helpers
import Minesweeper.Model
import Minesweeper.Update
import Minesweeper.View

main :: IO ()
main = run $ do
  gen <- getStdGen
  model <- liftIO (mkModel ModeBeginner gen)
  startComponent 
    (component model updateModel viewModel) 
      { events = defaultEvents <> pointerEvents
      -- , logLevel = DebugAll
      }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

