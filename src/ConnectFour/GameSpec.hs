
module ConnectFour.GameSpec (spec) where

import Data.Maybe (fromMaybe)
import Test.Hspec

import Game
import ConnectFour.Game

playMoves :: Game -> [Int] -> Game
playMoves = foldl (\g j -> fromMaybe g $ play (Move j) g)

spec :: Spec
spec = do

  describe "ConnectFour mkGame" $ do
    let g = mkGame
    it "getNiNj" $ getNiNj `shouldBe` (8, 8)
    it "getStatus" $ getStatus g `shouldBe` RedPlays
    it "getCurrentPlayer" $ getCurrentPlayer g `shouldBe` PlayerRed
    it "isRunning" $ isRunning g `shouldBe` True
    it "getPossibleMoves" $ getPossibleMoves g `shouldBe` map Move [7, 6 .. 0]

  describe "ConnectFour reset" $ do
    let g = reset mkGame
    it "getStatus" $ getStatus g `shouldBe` YellowPlays
    it "getCurrentPlayer" $ getCurrentPlayer g `shouldBe` PlayerYellow
    it "isRunning" $ isRunning g `shouldBe` True
    it "getPossibleMoves" $ getPossibleMoves g `shouldBe` map Move [7, 6 .. 0]

  describe "ConnectFour play 2" $ do
    case play (Move 2) mkGame of
      Nothing -> it "play 2 returns Nothing" False
      Just g -> do
        it "getStatus" $ getStatus g `shouldBe` YellowPlays
        it "getCurrentPlayer" $ getCurrentPlayer g `shouldBe` PlayerYellow
        it "isRunning" $ isRunning g `shouldBe` True
        it "getPossibleMoves" $ getPossibleMoves g `shouldBe` map Move [7, 6 .. 0]

  describe "ConnectFour play, red wins vertical" $ do
    let g = playMoves mkGame [1, 3, 1, 3, 1, 3, 1] 
    it "getStatus" $ getStatus g `shouldBe` RedWins
    it "getCurrentPlayer" $ getCurrentPlayer g `shouldBe` PlayerRed
    it "isRunning" $ isRunning g `shouldBe` False
    it "getPossibleMoves" $ getPossibleMoves g `shouldBe`  []

-- TODO wins horizontal
-- TODO wins diagonal 1
-- TODO wins diagonal 2
-- TODO draw

