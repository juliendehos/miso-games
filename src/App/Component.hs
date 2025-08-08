{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
    "Breakthrough" -> do
      mGameType .= Breakthrough
      io_ $ consoleLog "Breakthrough"
    "Minesweeper" -> do
      mGameType .= Minesweeper
      io_ $ consoleLog "Minesweeper"
    "Tictactoe" -> do
      mGameType .= Tictactoe
      io_ $ consoleLog "Tictactoe"
    _ -> do
      io_ $ consoleLog "unknown game"

-------------------------------------------------------------------------------
-- View
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel Model{..} = 
  div_ [] 
    [ h1_ [] [ "miso-games" ]
    , p_ [] 
        [ text "Game: "
        , select_ [ onChange ActionAskGame ]
            [ option_ [ selected_ (_mGameType == Breakthrough) ]  [ "Breakthrough" ]
            , option_ [ selected_ (_mGameType == Minesweeper) ]   [ "Minesweeper" ]
            , option_ [ selected_ (_mGameType == Tictactoe) ]     [ "Tictactoe" ]
            ]
        ]
    -- , p_ [] [ "player 2: TODO" ]
    , gameDiv
    -- , h2_ [] [ "test tictactoe" ]
    -- , div_ [] +> Tictactoe.mkComponent
    -- , h2_ [] [ "test minesweeper" ]
    -- , div_ [] +> Minesweeper.mkComponent _mGen
    ]

  where
    gameDiv = case _mGameType of
      Breakthrough -> 
        div_ []
          [ h2_ [] [ "Breakthrough" ]
          , div_ [] +> Breakthrough.mkComponent
          ]
      Minesweeper -> 
        div_ []
          [ h2_ [] [ "Minesweeper" ]
          , div_ [] +> Minesweeper.mkComponent _mGen   -- TODO update _mGen
          ]
      Tictactoe -> 
        div_ []
          [ h2_ [] [ "Tictactoe" ]
          , div_ [] +> Tictactoe.mkComponent
          ]

-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------

mkComponent :: Model -> App Model Action
mkComponent model = 
  (component model updateModel viewModel) 
    { events = defaultEvents <> pointerEvents
    }

