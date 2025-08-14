
module Breakthrough.GameSpec (spec) where

import Data.Maybe (fromMaybe)
import Test.Hspec

import Breakthrough.Game
import Game

playMoves :: Game -> [Move] -> Game
playMoves = foldl (\g m -> fromMaybe g $ play m g)

spec :: Spec
spec = do

  describe "Breakthrough status" $ do
    it "status 1" $ getStatus (mkGame 8 8) `shouldBe` RedPlays
    it "status 2" $ getStatus (reset $ mkGame 8 8) `shouldBe` BluePlays

  describe "Breakthrough play" $ do

    let g1 = playMoves (mkGame 8 8)
              [ Move (6, 2) (5, 2), Move (1, 5) (2, 5)
              , Move (5, 2) (4, 2), Move (2, 5) (3, 5)
              , Move (4, 2) (3, 2), Move (3, 5) (4, 5)
              , Move (3, 2) (2, 2), Move (4, 5) (5, 5)
              , Move (2, 2) (1, 1), Move (5, 5) (6, 4)
              , Move (1, 1) (0, 2)
              ]
    it "Red begins, Red wins" $ getStatus g1 `shouldBe` RedWins

    let g2 = playMoves (mkGame 8 8)
              [ Move (6, 3) (5, 4), Move (1, 1) (2, 2)
              , Move (7, 4) (6, 3), Move (2, 2) (3, 3)
              , Move (5, 4) (4, 4), Move (3, 3) (4, 4)
              , Move (6, 6) (5, 6), Move (4, 4) (5, 4)
              , Move (6, 1) (5, 1), Move (5, 4) (6, 5)
              , Move (5, 6) (4, 6), Move (6, 5) (7, 4)
              ]
    it "Red begins, Blue wins" $ getStatus g2 `shouldBe` BlueWins

  describe "Breakthrough 8x6" $ do

    let g1 = playMoves (mkGame 8 6)
              [ Move (6, 2) (5, 2), Move (1, 5) (2, 5)
              , Move (5, 2) (4, 2), Move (2, 5) (3, 5)
              , Move (4, 2) (3, 2), Move (3, 5) (4, 5)
              , Move (3, 2) (2, 2), Move (4, 5) (5, 5)
              , Move (2, 2) (1, 1), Move (5, 5) (6, 4)
              , Move (1, 1) (0, 2)
              ]
    it "Red begins, Red wins" $ getStatus g1 `shouldBe` RedWins

    let g2 = playMoves (mkGame 8 6)
              [ Move (6, 3) (5, 4), Move (1, 1) (2, 2)
              , Move (7, 4) (6, 3), Move (2, 2) (3, 3)
              , Move (5, 4) (4, 4), Move (3, 3) (4, 4)
              , Move (6, 5) (5, 5), Move (4, 4) (5, 4)
              , Move (6, 1) (5, 1), Move (5, 4) (6, 5)
              , Move (5, 5) (4, 5), Move (6, 5) (7, 4)
              ]
    it "Red begins, Blue wins" $ getStatus g2 `shouldBe` BlueWins

    let g3 = playMoves (mkGame 8 6)
              [ Move (6, 1) (5, 1), Move (1, 0) (2, 1)
              , Move (5, 1) (4, 1), Move (2, 1) (3, 2)
              , Move (4, 1) (3, 2), Move (1, 1) (2, 1)
              , Move (3, 2) (2, 1), Move (0, 0) (1, 0)
              , Move (2, 1) (1, 0), Move (0, 1) (1, 0)
              , Move (6, 0) (5, 1), Move (1, 0) (2, 1)
              , Move (6, 2) (5, 2), Move (2, 1) (3, 1)
              , Move (6, 3) (5, 4), Move (3, 1) (4, 1)
              , Move (5, 2) (4, 1), Move (1, 2) (2, 2)
              , Move (6, 4) (5, 3), Move (2, 2) (3, 2)
              , Move (4, 1) (3, 2), Move (1, 3) (2, 3)
              , Move (3, 2) (2, 3), Move (0, 2) (1, 2)
              , Move (2, 3) (1, 4), Move (0, 5) (1, 4)
              , Move (5, 4) (4, 4), Move (0, 3) (1, 3)
              , Move (6, 5) (5, 5), Move (1, 4) (2, 4)
              , Move (5, 3) (4, 3), Move (2, 4) (3, 4)
              , Move (4, 3) (3, 4), Move (0, 4) (1, 4)
              , Move (5, 1) (4, 2), Move (1, 3) (2, 3)
              , Move (3, 4) (2, 3), Move (1, 5) (2, 5)
              , Move (2, 3) (1, 4), Move (2, 5) (3, 5)
              , Move (4, 4) (3, 5), Move (1, 2) (2, 2)
              , Move (5, 5) (4, 5), Move (2, 2) (3, 3)
              , Move (4, 2) (3, 3)
              ]
    it "Red begins, Red wins because blue has no pieces left" $ getStatus g3 `shouldBe` RedWins

