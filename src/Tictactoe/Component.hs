{-# LANGUAGE OverloadedStrings #-}

module Tictactoe.Component (mkComponent) where

import Miso
import Miso.Canvas (Canvas, canvas, clearRect)
import Miso.Style qualified as Style
import Miso.String (ms)

import Helpers.Canvas

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

canvasSizeD :: Double
canvasSizeD = 300

cellSize :: Int
cellSize = round (canvasSizeD / 3)

-------------------------------------------------------------------------------
-- model
-------------------------------------------------------------------------------

type Model = ()

-------------------------------------------------------------------------------
-- action
-------------------------------------------------------------------------------

-- data Action = ActionAskPlay PointerEvent

type Action = ()

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: Component m Model Action
mkComponent = 
  (component () updateModel viewModel)
    { events = defaultEvents <> pointerEvents
    }

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Applicative f => Action -> f Action
updateModel () = pure ()

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View parent action
viewModel _ =
  div_ [] 
    [ canvas 
        [ width_ (ms $ show canvasSizeD)
        , height_ (ms $ show canvasSizeD)
        , Style.style_  [Style.border "2px solid black"]
        -- , onPointerUp ActionAskPlay
        ]
      initCanvas
      (drawCanvas ())
    ]

initCanvas :: DOMRef -> Canvas ()
initCanvas _ = pure ()

drawCanvas :: Model -> Action -> Canvas ()
drawCanvas () () = do
  clearRect (0, 0, canvasSizeD, canvasSizeD)
  drawBackground (Style.Hex "88DD88") canvasSizeD canvasSizeD
  drawGrid Style.black 3 3 cellSize cellSize canvasSizeD canvasSizeD

