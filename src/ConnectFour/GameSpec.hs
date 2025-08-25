
module ConnectFour.GameSpec (spec) where

import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Test.Hspec

import Game
import ConnectFour.Game

playMoves :: Game -> [Move] -> Game
playMoves = foldl (\g m -> fromMaybe g $ play m g)

spec :: Spec
spec = do

  -- TODO

  describe "ConnectFour checkWin" $ do
    it "1" $ checkWin mkBoard CellRed 0 2 `shouldBe` False
    it "2" $ checkWin (mkBoard V.// [(0, CellRed), (1, CellRed), (3, CellRed)]) CellRed 0 2 `shouldBe` True
    it "3" $ checkWin (mkBoard V.// [(0, CellRed), (1, CellRed), (4, CellRed)]) CellRed 0 2 `shouldBe` False

