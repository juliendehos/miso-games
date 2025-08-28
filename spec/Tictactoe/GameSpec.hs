
module Tictactoe.GameSpec (spec) where

import Data.Maybe (fromMaybe)
import Test.Hspec

import Game
import Tictactoe.Game

playMoves :: Game -> [Move] -> Game
playMoves = foldl (\g m -> fromMaybe g $ play m g)

spec :: Spec
spec = do

  describe "Tictactoe status" $ do
    it "status 1" $ getStatus mkGame `shouldBe` XPlays
    it "status 2" $ getStatus (reset mkGame) `shouldBe` OPlays

  describe "Tictactoe play" $ do

    let g1 = playMoves mkGame 
              [ Move 1 1, Move 0 0
              , Move 2 0, Move 0 2
              , Move 0 1, Move 2 1
              , Move 1 0, Move 1 2
              , Move 2 2
              ]
    it "X begins, draw" $ getStatus g1 `shouldBe` Draw

    let g2 = playMoves mkGame 
              [ Move 1 1, Move 2 2
              , Move 0 2, Move 0 0
              , Move 1 0, Move 1 2
              , Move 0 1, Move 2 0
              , Move 2 1
              ]
    it "X begins, X wins, last move" $ getStatus g2 `shouldBe` XWins

    let g3 = playMoves mkGame 
              [ Move 1 1, Move 0 2
              , Move 0 0, Move 0 1
              , Move 2 2
              ]
    it "X begins, X wins" $ getStatus g3 `shouldBe` XWins

    let g4 = playMoves mkGame 
              [ Move 1 1, Move 0 2
              , Move 0 0, Move 1 2
              , Move 0 1, Move 2 2
              ]
    it "X begins, O wins" $ getStatus g4 `shouldBe` OWins

    let g5 = playMoves (reset mkGame) 
              [ Move 0 0, Move 0 2
              , Move 1 1, Move 1 2
              , Move 2 2
              ]
    it "O begins, O wins" $ getStatus g5 `shouldBe` OWins

    let g6 = playMoves (reset mkGame) 
              [ Move 1 1, Move 2 0
              , Move 0 0, Move 2 2
              , Move 1 0, Move 2 1
              ]
    it "O begins, X wins" $ getStatus g6 `shouldBe` XWins

    let g7 = playMoves mkGame 
              [ Move 0 0, Move 1 1
              , Move 0 1, Move 2 0
              , Move 0 2
              ]
    it "X wins, row" $ getStatus g7 `shouldBe` XWins

    let g8 = playMoves mkGame 
              [ Move 1 2, Move 0 0
              , Move 0 2, Move 0 1
              , Move 2 2
              ]
    it "X wins, col" $ getStatus g8 `shouldBe` XWins

    let g9 = playMoves mkGame 
              [ Move 2 2, Move 1 2
              , Move 0 0, Move 2 1
              , Move 1 1
              ]
    it "X wins, diag 1" $ getStatus g9 `shouldBe` XWins

    let g10 = playMoves mkGame 
              [ Move 1 1, Move 1 0
              , Move 0 2, Move 0 1
              , Move 2 0
              ]
    it "X wins, diag 2" $ getStatus g10 `shouldBe` XWins

