
module Helpers.BoardSpec (spec) where

import Test.Hspec

import Helpers.Board

spec :: Spec
spec = do
  describe "Board 1" $ do

    let b = mkBoardFromList 2 3 [1 .. 6::Int]
    it "getIJ" $ getIJ 1 0 b `shouldBe` 4

