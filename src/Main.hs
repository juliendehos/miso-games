
import Miso
import System.Random (getStdGen)

import App.Component (mkComponent)
import App.Model (mkAppModel)

main :: IO ()
main = run $ do
  model <- mkAppModel <$> getStdGen
  let appComponent = mkComponent model
  startComponent appComponent
    -- { logLevel = DebugAll }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

