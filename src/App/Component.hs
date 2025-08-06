{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Component where

import Miso

import Control.Monad.Primitive
import Miso.Lens
import Miso.Lens.TH
import System.Random.Stateful

import Minesweeper.Component as Tictactoe
import Tictactoe.Component as Minesweeper

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
  = ActionAskGame GameType

updateModel :: Action -> Transition Model Action
updateModel (ActionAskGame gt) = do
  case gt of
    Tictactoe -> io_ $ consoleLog "tictactoe"
    Minesweeper -> io_ $ consoleLog "minesweeper"
  mGameType .= gt

-------------------------------------------------------------------------------
-- View
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel Model{..} = 
  div_ [] 
    [ h1_ [] [ "miso-games" ]
    , p_ [] 
        [ text "game: "
        , select_ []
            [ option_ [ onClick (ActionAskGame Tictactoe) ] [ "Tictactoe" ]
            , option_ [ onClick (ActionAskGame Minesweeper) ] [ "Minesweeper" ]
            ]
        ]
    -- , p_ [] [ "player 2: TODO" ]
    , gameH2
    , gameDiv
    ]

  where
    gameDiv = case _mGameType of
      Tictactoe   -> div_ [] +> Tictactoe.mkComponent
      Minesweeper -> div_ [] +> Minesweeper.mkComponent   -- TODO gen
    gameH2 = case _mGameType of
      Tictactoe   -> h2_ [] [ "tictactoe" ]
      Minesweeper -> h2_ [] [ "minesweeper" ]

-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------

mkComponent :: Model -> App Model Action
mkComponent model = 
  (component model updateModel viewModel) 
    { events = defaultEvents <> pointerEvents
    }

