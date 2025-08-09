
module Tictactoe.GameSpec (spec) where

import Test.Hspec

import Tictactoe.Game

spec :: Spec
spec = do

  describe "Tictactoe 1" $ do

    it "status 1" $ getStatus mkGame `shouldBe` XPlays

    it "status 2" $ getStatus (reset mkGame) `shouldBe` OPlays

-- TODO

