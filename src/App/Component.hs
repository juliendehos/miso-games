{-# LANGUAGE OverloadedStrings #-}

module App.Component where

import Miso

import Miso.Lens

import App.Model
import Breakthrough.Component as Breakthrough
import Minesweeper.Component as Minesweeper
import Tictactoe.Component as Tictactoe

-------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------

newtype Action 
  = ActionAskGame MisoString

updateModel :: Action -> Transition Model Action
updateModel (ActionAskGame gt) = do
  case gt of
    "Breakthrough"  -> modelGameType .= Breakthrough
    "Minesweeper"   -> modelGameType .= Minesweeper
    "Tictactoe"     -> modelGameType .= Tictactoe
    _               -> pure ()

-------------------------------------------------------------------------------
-- View
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel model = 
  div_ [] 
    [ p_ [] 
        [ text "Game: "
        , select_ [ onChange ActionAskGame ]
            [ option_ [ selected_ (model^.modelGameType == Breakthrough) ]  [ "Breakthrough" ]
            , option_ [ selected_ (model^.modelGameType == Minesweeper) ]   [ "Minesweeper" ]
            , option_ [ selected_ (model^.modelGameType == Tictactoe) ]     [ "Tictactoe" ]
            ]
        ]
    -- , p_ [] [ "player 2: TODO" ]
    , gameDiv
    ]

  where
    gameDiv = case model^.modelGameType of
      Breakthrough -> 
        div_ []
          [ h2_ [] [ "Breakthrough" ]
          , div_ [ key_ ("Breakthrough"::MisoString) ] +> 
              Breakthrough.mkComponent
          ]
      Minesweeper -> 
        div_ []
          [ h2_ [] [ "Minesweeper" ]
          , div_ [ key_ ("Minesweeper"::MisoString) ] +> 
              Minesweeper.mkComponent (model^.modelGen)
          ]
      Tictactoe -> 
        div_ []
          [ h2_ [] [ "Tictactoe" ]
          , div_ [ key_ ("Tictactoe"::MisoString) ] +> 
              Tictactoe.mkComponent
          ]
-- TODO bidirectional component

-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------

mkComponent :: Model -> App Model Action
mkComponent model = component model updateModel viewModel

