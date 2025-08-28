
import Test.Hspec

import Breakthrough.GameSpec
import ConnectFour.GameSpec
import Tictactoe.GameSpec

main :: IO ()
main = hspec $ do
  Breakthrough.GameSpec.spec
  ConnectFour.GameSpec.spec
  Tictactoe.GameSpec.spec

