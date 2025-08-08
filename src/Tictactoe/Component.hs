{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tictactoe.Component (mkComponent) where

import Control.Monad (when)
import Miso
import Miso.Lens
import Miso.Canvas (Canvas, canvas)
import Miso.Canvas qualified as Canvas 
import Miso.Style qualified as Style

import Helpers.Canvas
import Tictactoe.Game
import Tictactoe.Model

-------------------------------------------------------------------------------
-- params and helpers
-------------------------------------------------------------------------------

bgColor, bgColorEnd, fgColor :: Style.Color
bgColor = Style.Hex "88DD88"
bgColorEnd = Style.Hex "DDDDDD"
fgColor = Style.red

cellSizeD, canvasSizeD :: Double
cellSizeD = 100
canvasSizeD = cellSizeD * 3

cellSize :: Int
cellSize = round cellSizeD

xy2ij' :: Double -> Double -> (Int, Int)
xy2ij' = xy2ij cellSize cellSize

ij2xyC' :: Int -> Int -> (Double, Double)
ij2xyC' = ij2xyC cellSize cellSize

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
  game <- use modelGame
  when (isRunning game && button event == 0) $ do
    player <- getCurrentPlayer <$> use modelGame
    let (i, j) = uncurry xy2ij' $ offset event 
        iStr = ms $ show i
        jStr = ms $ show j
        pStr = case player of
          PlayerX -> "X"
          PlayerO -> "O"
    case play (Move i j) game of
      Nothing -> modelLog .= pStr <> " failed to play " <> iStr <> " " <> jStr
      Just game' -> do
        modelLog .= pStr <> " played " <> iStr <> " " <> jStr
        modelGame .= game'

updateModel ActionNewGame = do
  modelGame %= Tictactoe.Game.reset
  modelLog .= "new game"

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View parent Action
viewModel model@Model{..} =
  div_ [] 
    [ p_ [] [ button_ [ onClick ActionNewGame ] [ "new game" ] ]
    , canvas 
        [ width_ (ms $ show canvasSizeD)
        , height_ (ms $ show canvasSizeD)
        , Style.style_  [Style.border "2px solid black"]
        , onPointerUp ActionAskPlay
        ]
      initCanvas
      (drawCanvas model)
    , p_ [] [ text ("status: " <> fmtStatus (getStatus _modelGame)) ]
    , p_ [] [ text ("log: " <> _modelLog) ]
    , p_ [] [ text ("nb possible moves: " <> ms (show nbPossibleMoves)) ]
    ]

  where
    nbPossibleMoves = length $ getMoves _modelGame

    fmtStatus = \case
      XPlays  -> "X plays"
      OPlays  -> "O plays"
      XWins   -> "X wins"
      OWins   -> "O wins"
      Draw    -> "it's a draw"

initCanvas :: DOMRef -> Canvas ()
initCanvas _ = pure ()

drawCanvas :: Model -> () -> Canvas ()
drawCanvas Model{..} () = do
  Canvas.clearRect (0, 0, canvasSizeD, canvasSizeD)
  let bg = getBgColor _modelGame
  drawBackground bg canvasSizeD canvasSizeD
  drawGrid Style.black 3 3 cellSize cellSize canvasSizeD canvasSizeD
  forGame _modelGame (drawGameCell bg)

drawGameCell :: Style.Color -> Int -> Int -> Cell -> Canvas ()
drawGameCell bg i j = \case
  CellX -> drawX i j
  CellO -> drawO bg i j
  _ -> pure ()


drawX :: Int -> Int -> Canvas ()
drawX i j = do

  Canvas.fillStyle (Canvas.color fgColor)

  Canvas.save ()
  Canvas.translate $ ij2xyC' i j
  Canvas.rotate (pi * 0.25)
  Canvas.fillRect (-cs005, -cs04, cs01, cs08)
  Canvas.restore ()

  Canvas.save ()
  Canvas.translate $ ij2xyC' i j
  Canvas.rotate (pi * (-0.25))
  Canvas.fillRect (-cs005, -cs04, cs01, cs08)
  Canvas.restore ()

drawO :: Style.Color -> Int -> Int -> Canvas ()
drawO bg i j = do

  Canvas.save ()
  Canvas.translate $ ij2xyC' i j

  Canvas.beginPath ()
  Canvas.fillStyle (Canvas.color fgColor)
  Canvas.arc (0, 0, cs03, 0, 2*pi)
  Canvas.fill ()

  Canvas.beginPath ()
  Canvas.fillStyle (Canvas.color bg)
  Canvas.arc (0, 0, cs02, 0, 2*pi)
  Canvas.fill ()

  Canvas.restore ()

getBgColor :: Game -> Style.Color
getBgColor game = if isRunning game then bgColor else bgColorEnd

cs005, cs01, cs02, cs03, cs04, cs08 :: Double
cs005 = cellSizeD * 0.05
cs01 = cellSizeD * 0.1
cs02 = cellSizeD * 0.2
cs03 = cellSizeD * 0.3
cs04 = cellSizeD * 0.4
cs08 = cellSizeD * 0.8

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: Component m Model Action
mkComponent = do
  let initialModel = mkModel
  (component initialModel updateModel viewModel)
    { events = defaultEvents <> pointerEvents
    }

