
import Test.Hspec

import Helpers.BoardSpec
import Tictactoe.GameSpec

main :: IO ()
main = hspec $ do
  Helpers.BoardSpec.spec
  Tictactoe.GameSpec.spec

