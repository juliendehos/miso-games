{-# LANGUAGE OverloadedStrings #-}

module App.Component where

import Miso

import Miso.Lens
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P

import App.Model
import Breakthrough.Component as Breakthrough
import Minesweeper.Component as Minesweeper
import Tictactoe.Component as Tictactoe

-------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------

newtype Action 
  = ActionAskGame MisoString

updateModel :: Action -> Transition AppModel Action
updateModel (ActionAskGame gt) = do
  case gt of
    "Breakthrough"      -> modelGameType .= Breakthrough
    "Breakthrough 8x6"  -> modelGameType .= Breakthrough86
    "Minesweeper"       -> modelGameType .= Minesweeper
    "Tictactoe"         -> modelGameType .= Tictactoe
    _                   -> pure ()

-------------------------------------------------------------------------------
-- View
-------------------------------------------------------------------------------

viewModel :: AppModel -> View AppModel Action
viewModel model = 
  div_ [] 
    [ p_ [] 
        [ text "game: "
        , select_ [ onChange ActionAskGame ]
            [ option_ [ selected_ (model^.modelGameType == Breakthrough) ]    [ "Breakthrough" ]
            , option_ [ selected_ (model^.modelGameType == Breakthrough86) ]  [ "Breakthrough 8x6" ]
            , option_ [ selected_ (model^.modelGameType == Minesweeper) ]     [ "Minesweeper" ]
            , option_ [ selected_ (model^.modelGameType == Tictactoe) ]       [ "Tictactoe" ]
            ]
        ]
    , gameDiv
    ]

  where
    fmtInfo name url = p_ [] [ text "you're playing " , a_ [href_ url] [name] ]

    gameDiv = case model^.modelGameType of
      Breakthrough -> 
        div_ []
          [ fmtInfo "Breakthrough" "https://en.wikipedia.org/wiki/Breakthrough_(board_game)"
          , div_ [ key_ ("Breakthrough"::MisoString) ] +> 
              (model^.modelBreakthrough & Breakthrough.mkComponent)
                { bindings = [ modelBreakthrough <--> this ] }
          ]
      Breakthrough86 -> 
        div_ []
          [ fmtInfo "Breakthrough 8x6" "https://en.wikipedia.org/wiki/Breakthrough_(board_game)"
          , div_ [ key_ ("Breakthrough86"::MisoString) ] +> 
              (model^.modelBreakthrough86 & Breakthrough.mkComponent)
                { bindings = [ modelBreakthrough86 <--> this ] }
          ]
      Minesweeper -> 
        div_ []
          [ fmtInfo "Minesweeper" "https://en.wikipedia.org/wiki/Minesweeper" 
          , div_ [ key_ ("Minesweeper"::MisoString) ] +> 
              (model^.modelMinesweeper & Minesweeper.mkComponent)
                { bindings = [ modelMinesweeper <--> this ] }
          ]
      Tictactoe -> 
        div_ []
          [ fmtInfo "Tictactoe" "https://en.wikipedia.org/wiki/Tic-tac-toe" 
          , div_ [ key_ ("Tictactoe"::MisoString) ] +> 
              (model^.modelTictactoe & Tictactoe.mkComponent)
                { bindings = [ modelTictactoe <--> this ] }
          ]

-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------

mkComponent :: AppModel -> App AppModel Action
mkComponent model = component model updateModel viewModel

