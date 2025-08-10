{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Breakthrough.Component (mkComponent) where

import Control.Monad (when)
import Miso
import Miso.Lens
import Miso.Canvas (Canvas, canvas)
import Miso.Canvas qualified as Canvas 
import Miso.Style qualified as Style

import Helpers.Canvas
import Breakthrough.Game
import Breakthrough.Model

-------------------------------------------------------------------------------
-- params and helpers
-------------------------------------------------------------------------------

bgColor, bgColorEnd :: Style.Color
bgColor = Style.Hex "88DD88"
bgColorEnd = Style.Hex "DDDDDD"

cellSizeD, canvasSizeD :: Double
cellSizeD = 20
canvasSizeD = cellSizeD * 3

cellSize :: Int
cellSize = round cellSizeD

xy2ij :: Double -> Double -> (Int, Int)
xy2ij = xy2ij' cellSize cellSize

ij2xyC :: Int -> Int -> (Double, Double)
ij2xyC = ij2xyC' cellSize cellSize


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
    -- , logLevel = DebugAll
    }


