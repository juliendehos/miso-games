
import Control.Monad.IO.Class (liftIO)
import Miso
import System.Random (getStdGen)

import App.Component (mkComponent)
import App.Model (mkModel)

main :: IO ()
main = run $ do
  gen <- getStdGen
  model <- liftIO (mkModel gen)
  let appComponent = mkComponent model
  startComponent appComponent
    { logLevel = DebugAll }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

