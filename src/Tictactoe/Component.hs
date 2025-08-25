{-# LANGUAGE OverloadedStrings #-}

module Tictactoe.Component (mkComponent) where

import Control.Monad (when, forM_)
import Data.Maybe (fromMaybe)
import Miso
import Miso.Lens
import Miso.Canvas (Canvas, canvas)
import Miso.Canvas qualified as Canvas 
import Miso.CSS qualified as CSS
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P

import Game
import Helpers.Canvas
import Tictactoe.Game
import Tictactoe.Model

-------------------------------------------------------------------------------
-- view params
-------------------------------------------------------------------------------

bgColor, bgColorEnd, fgColor :: CSS.Color
bgColor = CSS.hex 0x88DD88
bgColorEnd = CSS.hex 0xDDDDDD
fgColor = CSS.red

cellSize :: Int
cellSize = 100

cellSizeD :: Double
cellSizeD = fromIntegral cellSize

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
  | ActionAskPlayerO MisoString
  | ActionNewGame

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect parentModel Model Action

updateModel (ActionAskPlayerO pt) = do
  modelLog .= pt <> " plays O"
  case pt of
    "Human"   -> modelPlayerO .= Human
    "Random"  -> modelPlayerO .= BotRandom >> tryPlayBotO
    "McEasy"  -> modelPlayerO .= BotMcEasy >> tryPlayBotO
    "McHard"  -> modelPlayerO .= BotMcHard >> tryPlayBotO
    _         -> pure ()

updateModel ActionNewGame = do
  modelGame %= Tictactoe.Game.reset
  modelLog .= "new game"
  tryPlayBotO

updateModel (ActionAskPlay event) = do
  game <- use modelGame
  when (isRunning game && button event == 0) $ do
    let (i, j) = uncurry xy2ij $ offset event 
    case getCurrentPlayer game of
      PlayerX -> do
        doPlay PlayerX (Move i j)
        tryPlayBotO
      PlayerO -> do
        moveO <- fromMaybe (Move i j) <$> genMovePlayerO
        doPlay PlayerO moveO

doPlay :: Player -> Move -> Effect parentModel Model Action
doPlay player move@(Move i j) = do
  game <- use modelGame
  case play move game of
    Nothing -> modelLog .= fmtLogPlay player False i j
    Just game' -> do
      modelLog .= fmtLogPlay player True i j
      modelGame .= game'

tryPlayBotO :: Effect parentModel Model Action
tryPlayBotO = do
  game <- use modelGame
  let player = getCurrentPlayer game
  when (isRunning game && player == PlayerO) $ do
    mMoveO <- genMovePlayerO
    forM_ mMoveO (doPlay PlayerO)

fmtLogPlay :: Player -> Bool -> Int -> Int -> MisoString
fmtLogPlay p ok i j =
  let pStr = if p == PlayerX then "X" else "O"
      okStr = if ok then "played" else "failed to play"
      iStr = ms $ show i
      jStr = ms $ show j
  in pStr <> " " <> okStr <> " " <> iStr <> " " <> jStr

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View parent Action
viewModel model =
  div_ [] 
    [ p_ [] 
        [ text "player O: "
        , select_ [ onChange ActionAskPlayerO ]
            [ option_ [ selected_ (model^.modelPlayerO == Human) ]      [ "Human" ]
            , option_ [ selected_ (model^.modelPlayerO == BotRandom) ]  [ "Random" ]
            , option_ [ selected_ (model^.modelPlayerO == BotMcEasy) ]  [ "McEasy" ]
            , option_ [ selected_ (model^.modelPlayerO == BotMcHard) ]  [ "McHard" ]
            ]
        ]
    , p_ [] [ button_ [ onClick ActionNewGame ] [ "new game" ] ]
    , canvas 
        [ width_ (ms $ show canvasWidthD)
        , height_ (ms $ show canvasHeightD)
        , CSS.style_  [CSS.border "2px solid black"]
        , onPointerUp ActionAskPlay
        ]
      initCanvas
      (drawCanvas ni nj canvasWidthD canvasHeightD model)
    , p_ [] 
        [ text ("status: " <> (model^.modelGame & getStatus & fmtStatus))
        , br_ []
        , text ("nb possible moves: " <> ms (show nbPossibleMoves))
        , br_ []
        , text ("log: " <> model^.modelLog)
        ]
    ]

  where
    (ni, nj) = getNiNj
    canvasWidthD = fromIntegral $ nj*cellSize
    canvasHeightD = fromIntegral $ ni*cellSize

    nbPossibleMoves = model^.modelGame & getPossibleMoves & length

    fmtStatus = \case
      XPlays  -> "X plays"
      OPlays  -> "O plays"
      XWins   -> "X wins"
      OWins   -> "O wins"
      Draw    -> "it's a draw"

initCanvas :: DOMRef -> Canvas ()
initCanvas _ = pure ()

drawCanvas :: Int -> Int -> Double -> Double -> Model -> () -> Canvas ()
drawCanvas ni nj canvasWidthD canvasHeightD model () = do
  Canvas.clearRect (0, 0, canvasWidthD, canvasHeightD)
  let bg = if model^.modelGame & isRunning then bgColor else bgColorEnd
  drawBackground bg canvasWidthD canvasHeightD
  drawGrid CSS.black nj ni cellSize cellSize canvasWidthD canvasHeightD
  forGame (model^.modelGame) (drawGameCell bg)

drawGameCell :: CSS.Color -> Int -> Int -> Cell -> Canvas ()
drawGameCell bg i j = \case
  CellX -> drawX i j
  CellO -> drawO bg i j
  _ -> pure ()


drawX :: Int -> Int -> Canvas ()
drawX i j = do

  Canvas.fillStyle (Canvas.color fgColor)

  Canvas.save ()
  Canvas.translate $ ij2xyC i j
  Canvas.rotate (pi * 0.25)
  Canvas.fillRect (-cs005, -cs04, cs01, cs08)
  Canvas.restore ()

  Canvas.save ()
  Canvas.translate $ ij2xyC i j
  Canvas.rotate (pi * (-0.25))
  Canvas.fillRect (-cs005, -cs04, cs01, cs08)
  Canvas.restore ()

drawO :: CSS.Color -> Int -> Int -> Canvas ()
drawO bg i j = do

  Canvas.save ()
  Canvas.translate $ ij2xyC i j

  Canvas.beginPath ()
  Canvas.fillStyle (Canvas.color fgColor)
  Canvas.arc (0, 0, cs03, 0, 2*pi)
  Canvas.fill ()

  Canvas.beginPath ()
  Canvas.fillStyle (Canvas.color bg)
  Canvas.arc (0, 0, cs02, 0, 2*pi)
  Canvas.fill ()

  Canvas.restore ()

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

mkComponent :: Model -> Component m Model Action
mkComponent model = 
  (component model updateModel viewModel)
    { events = defaultEvents <> pointerEvents
    -- , logLevel = DebugAll
    }

