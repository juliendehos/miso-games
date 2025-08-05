
import Control.Monad.IO.Class (liftIO)
import Miso
import System.Random (getStdGen)

import App.Component
import App.Model

main :: IO ()
main = run $ do
  gen <- getStdGen
  model <- liftIO (mkModel gen)
  let component = mkComponent model
  startComponent component
    { logLevel = DebugAll }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

