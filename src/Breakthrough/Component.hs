{-# LANGUAGE OverloadedStrings #-}

module Breakthrough.Component (mkComponent) where

import Control.Monad (forM_, when)
import Miso
import Miso.Lens
import Miso.Canvas (Canvas, canvas)
import Miso.Canvas qualified as Canvas 
import Miso.Style qualified as Style

import Helpers.Canvas
import Breakthrough.Game
import Breakthrough.Model

-------------------------------------------------------------------------------
-- view params
-------------------------------------------------------------------------------

bgColor, bgColorEnd :: Style.Color
bgColor = Style.Hex "88DD88"
bgColorEnd = Style.Hex "DDDDDD"

cellSize :: Int
cellSize = 70

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

xy2ij :: Double -> Double -> (Int, Int)
xy2ij = xy2ij' cellSize cellSize

ij2xyC :: Int -> Int -> (Double, Double)
ij2xyC = ij2xyC' cellSize cellSize

-------------------------------------------------------------------------------
-- action
-------------------------------------------------------------------------------

data Action 
  = ActionAskPlay PointerEvent
  | ActionNewGame

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect parentModel Model Action

updateModel (ActionAskPlay event) = do
  pure ()   -- TODO

updateModel ActionNewGame = do
  modelGame %= Breakthrough.Game.reset
  modelLog .= "new game"

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

cellSizeD :: Double
cellSizeD = fromIntegral cellSize

viewModel :: Model -> View parent Action
viewModel model =
  div_ [] 
    [ p_ [] [ button_ [ onClick ActionNewGame ] [ "new game" ] ]
    , canvas 
        [ width_ (ms $ show canvasWidthD)
        , height_ (ms $ show canvasHeightD)
        , Style.style_  [Style.border "2px solid black"]
        , onPointerUp ActionAskPlay
        ]
      initCanvas
      (drawCanvas ni nj canvasWidthD canvasHeightD model)
    , p_ [] [ text ("status: " <> (model^.modelGame & getStatus & fmtStatus)) ]
    , p_ [] [ text ("log: " <> model^.modelLog) ]
    ]

  where
    (ni, nj) = model^.modelGame & getNiNj
    canvasWidthD = fromIntegral $ nj*cellSize
    canvasHeightD = fromIntegral $ ni*cellSize

    fmtStatus = \case
      RedPlays  -> "Red plays"
      BluePlays -> "Blue plays"
      RedWins   -> "Red wins"
      BlueWins  -> "Blue wins"

initCanvas :: DOMRef -> Canvas ()
initCanvas _ = pure ()

drawCanvas :: Int -> Int -> Double -> Double -> Model -> () -> Canvas ()
drawCanvas ni nj canvasWidthD canvasHeightD model () = do
  Canvas.clearRect (0, 0, canvasWidthD, canvasHeightD)
  let bg = if model^.modelGame & isRunning then bgColor else bgColorEnd
  drawBackground bg canvasWidthD canvasHeightD
  drawGrid Style.black ni nj cellSize cellSize canvasWidthD canvasHeightD
  drawMoves model
  forGame (model^.modelGame) drawGameCell 

drawMoves :: Model -> Canvas ()
drawMoves model = 
  forM_ (model^.modelGame & getMovesFrom) $ uncurry (drawDisc Style.white cs045)

drawGameCell :: Int -> Int -> Cell -> Canvas ()
drawGameCell i j = \case
  CellRed   -> drawDisc Style.red cs04 i j
  CellBlue  -> drawDisc Style.blue cs04 i j
  _ -> pure ()

drawDisc :: Style.Color -> Double -> Int -> Int -> Canvas ()
drawDisc col rad i j = do
  Canvas.save ()
  Canvas.translate $ ij2xyC i j
  Canvas.beginPath ()
  Canvas.fillStyle (Canvas.color col)
  Canvas.arc (0, 0, rad, 0, 2*pi)
  Canvas.fill ()
  Canvas.restore ()

cs04, cs045 :: Double
cs04 = cellSizeD * 0.4
cs045 = cellSizeD * 0.45

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


