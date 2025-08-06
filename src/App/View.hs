{-# LANGUAGE OverloadedStrings #-}

module App.View where

import Miso
-- import System.Random.Stateful

import App.Model
import App.Update
-- import Minesweeper.Component as Minesweeper
-- import Minesweeper.Helpers as Minesweeper

import Tictactoe.Component as Tictactoe

viewModel :: Model -> View Model Action
viewModel _model = 
  div_ [] 
    [ h1_ [] [ "miso-games" ]
    , p_ [] [ "game: TODO" ]
    , p_ [] [ "player 2: TODO" ]
    , div_ [] +> tictactoeComponent
    ]

  where
    tictactoeComponent = Tictactoe.mkComponent

  {-
    -- minesweeperComponent = Minesweeper.mkComponent gen
    minesweeperComponent = 
      let initialModel = runST $ mkModel ModeBeginner gen
      in (component initialModel updateModel viewModel) 
        { events = defaultEvents <> pointerEvents
        }
    -}
