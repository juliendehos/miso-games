
module Helpers.BoardSpec (spec) where

import Test.Hspec

import Helpers.Board

spec :: Spec
spec = do

  describe "mkBoardFromList" $ do
    let b = mkBoardFromList 2 3 [1 .. 6::Int]
    it "getIJ 0 0" $ getIJ 0 0 b `shouldBe` 1
    it "getIJ 0 1" $ getIJ 0 1 b `shouldBe` 2
    it "getIJ 0 2" $ getIJ 0 2 b `shouldBe` 3
    it "getIJ 1 0" $ getIJ 1 0 b `shouldBe` 4
    it "getIJ 1 1" $ getIJ 1 1 b `shouldBe` 5
    it "getIJ 1 2" $ getIJ 1 2 b `shouldBe` 6

  describe "mkBoardFromVal" $ do
    let b = mkBoardFromVal 2 2 (42::Int)
    it "getIJ 0 0" $ getIJ 0 0 b `shouldBe` 42
    it "getIJ 0 1" $ getIJ 0 1 b `shouldBe` 42
    it "getIJ 1 0" $ getIJ 1 0 b `shouldBe` 42
    it "getIJ 1 1" $ getIJ 1 1 b `shouldBe` 42

  describe "boardNi boardNj" $ do
    let b = mkBoardFromVal 6 8 (42::Int)
    it "boardNi" $ _boardNi b `shouldBe` 6
    it "boardNj" $ _boardNj b `shouldBe` 8

  describe "setIJ" $ do
    let b = mkBoardFromVal 6 8 (42::Int)
    it "setIJ 1 2 37" $ getIJ 1 2 (setIJ 1 2 37 b) `shouldBe` 37

  describe "setIJs" $ do
    let b = mkBoardFromVal 6 8 (42::Int)
    let b' = setIJs [((1, 2), 13), ((2, 1), 37)] b
    it "1 2" $ getIJ 1 2 b' `shouldBe` 13
    it "2 1" $ getIJ 2 1 b' `shouldBe` 37

