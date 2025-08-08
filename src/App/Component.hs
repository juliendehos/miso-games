{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Component where

import Miso

import Control.Monad.Primitive
import Miso.Lens
import Miso.Lens.TH
import System.Random.Stateful

import Minesweeper.Component as Minesweeper
import Tictactoe.Component as Tictactoe

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data GameType
  = Tictactoe
  | Minesweeper
  deriving (Eq)

data Model = Model
  { _mGameType :: GameType
  , _mGen :: StdGen
  } deriving (Eq)

makeLenses ''Model

mkModel :: (PrimMonad m) => StdGen -> m Model
mkModel = pure . Model Tictactoe

-------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------

newtype Action 
  = ActionAskGame MisoString

updateModel :: Action -> Transition Model Action
updateModel (ActionAskGame gt) = do
  case gt of
    "Tictactoe" -> do
      mGameType .= Tictactoe
      io_ $ consoleLog "tictactoe"
    "Minesweeper" -> do
      mGameType .= Minesweeper
      io_ $ consoleLog "minesweeper"
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
        [ text "game: "
        , select_ [ onChange ActionAskGame ]
            [ option_ [] [ "Tictactoe" ]
            , option_ [] [ "Minesweeper" ]
            ]
        ]
    -- , p_ [] [ "player 2: TODO" ]
    , gameDiv
    , h2_ [] [ "test tictactoe" ]
    , div_ [] +> Tictactoe.mkComponent
    , h2_ [] [ "test minesweeper" ]
    , div_ [] +> Minesweeper.mkComponent _mGen
    ]

  where
    gameDiv = case _mGameType of
      Tictactoe -> 
        div_ []
          [ h2_ [] [ "Tictactoe" ]
          , div_ [] +> Tictactoe.mkComponent
          ]
      Minesweeper -> 
        div_ []
          [ h2_ [] [ "Minesweeper" ]
          , div_ [] +> Minesweeper.mkComponent _mGen   -- TODO update _mGen
          ]

-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------

mkComponent :: Model -> App Model Action
mkComponent model = 
  (component model updateModel viewModel) 
    { events = defaultEvents <> pointerEvents
    }

