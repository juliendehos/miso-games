
module Breakthrough.GameSpec (spec) where

import Test.Hspec

import Breakthrough.Game

spec :: Spec
spec = do

  describe "Breakthrough 1" $ do

    it "status 1" $ getStatus (mkGame 8 8) `shouldBe` RedPlays

    it "status 2" $ getStatus (reset $ mkGame 8 8) `shouldBe` BluePlays


-- TODO


