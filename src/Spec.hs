
import Test.Hspec

import Breakthrough.GameSpec
import Tictactoe.GameSpec

main :: IO ()
main = hspec $ do
  Breakthrough.GameSpec.spec
  Tictactoe.GameSpec.spec

