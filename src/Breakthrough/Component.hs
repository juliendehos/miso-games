{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Breakthrough.Component (mkComponent) where

import Miso

import Breakthrough.Game
import Breakthrough.Model

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

type Action = ()

updateModel :: Action -> Effect parentModel Model Action
updateModel () = pure ()

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View parent Action
viewModel _model =
  div_ []
    [ p_ [] [ "under construction..." ]
    ]

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: Component m Model Action
mkComponent = do
  let initialModel = mkModel
  (component initialModel updateModel viewModel)
    { events = defaultEvents <> pointerEvents
    }


