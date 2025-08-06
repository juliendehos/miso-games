{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tictactoe.Component (mkComponent) where

import Miso
import Miso.Lens
import Miso.Lens.TH
import Miso.Canvas (Canvas, canvas)
import Miso.Canvas qualified as Canvas 
import Miso.Style qualified as Style

import Helpers.Canvas
import Tictactoe.Game

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

bgColor, bgColorEnd, fgColor :: Style.Color
bgColor = Style.Hex "88DD88"
bgColorEnd = Style.Hex "DDDDDD"
fgColor = Style.red

cellSizeD, canvasSizeD :: Double
cellSizeD = 100
canvasSizeD = cellSizeD * 3

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

cellSize :: Int
cellSize = round cellSizeD

xy2ij' :: Double -> Double -> (Int, Int)
xy2ij' = xy2ij cellSize cellSize

ij2xy' :: Int -> Int -> (Double, Double)
ij2xy' i j = 
  let (x, y) = ij2xy cellSize cellSize i j
  in (x + cellSizeD*0.5, y + cellSizeD*0.5)

cs005, cs01, cs02, cs03, cs04, cs08 :: Double
cs005 = cellSizeD * 0.05
cs01 = cellSizeD * 0.1
cs02 = cellSizeD * 0.2
cs03 = cellSizeD * 0.3
cs04 = cellSizeD * 0.4
cs08 = cellSizeD * 0.8

-------------------------------------------------------------------------------
-- action
-------------------------------------------------------------------------------

data Action 
  = ActionAskPlay PointerEvent
  | ActionNewGame

-------------------------------------------------------------------------------
-- model
-------------------------------------------------------------------------------

data Model = Model
  { _modelGame  :: Game
  , _modelLog   :: MisoString
  } deriving (Eq)

mkModel :: Model
mkModel = Model mkGame "this is Tictactoe"

makeLenses ''Model

{-
modelGame :: Lens Model Game
modelGame = lens _modelGame $ \record field -> record { _modelGame = field }

modelLog :: Lens Model MisoString
modelLog = lens _modelLog $ \record field -> record { _modelLog = field }
-}

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect parentModel Model Action

updateModel (ActionAskPlay event) = do
  player <- getCurrentPlayer <$> use modelGame
  let (i, j) = uncurry xy2ij' $ offset event 
      iStr = ms $ show i
      jStr = ms $ show j
      pStr = case player of
        Player1 -> "X"
        Player2 -> "O"
  modelGame %= play (i, j)
  modelLog .= pStr <> " played " <> iStr <> " " <> jStr

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
    ]
  where
    fmtStatus = \case
      Player1Plays  -> "X plays"
      Player2Plays  -> "O plays"
      Player1Wins   -> "X wins"
      Player2Wins   -> "O wins"
      Draw          -> "it's a draw"

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
  CellPlayer1 -> drawX i j
  CellPlayer2 -> drawO bg i j
  _ -> pure ()


drawX :: Int -> Int -> Canvas ()
drawX i j = do

  Canvas.fillStyle (Canvas.color fgColor)

  Canvas.save ()
  Canvas.translate $ ij2xy' i j
  Canvas.rotate (pi * 0.25)
  Canvas.fillRect (-cs005, -cs04, cs01, cs08)
  Canvas.restore ()

  Canvas.save ()
  Canvas.translate $ ij2xy' i j
  Canvas.rotate (pi * (-0.25))
  Canvas.fillRect (-cs005, -cs04, cs01, cs08)
  Canvas.restore ()

drawO :: Style.Color -> Int -> Int -> Canvas ()
drawO bg i j = do

  Canvas.save ()
  Canvas.translate $ ij2xy' i j

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

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: Component m Model Action
mkComponent = do
  let initialModel = mkModel
  (component initialModel updateModel viewModel)
    { events = defaultEvents <> pointerEvents
    }

