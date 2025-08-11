
import Test.Hspec

import Helpers.BoardSpec
import Breakthrough.GameSpec
import Tictactoe.GameSpec

main :: IO ()
main = hspec $ do
  Helpers.BoardSpec.spec
  Breakthrough.GameSpec.spec
  Tictactoe.GameSpec.spec

