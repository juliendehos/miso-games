{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Othello.Component (mkComponent) where

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
import Othello.Game
import Othello.Model

-------------------------------------------------------------------------------
-- view params
-------------------------------------------------------------------------------

bgColor, bgColorEnd :: CSS.Color
bgColor = #88BBFF
bgColorEnd = #DDDDDD

cellSize :: Int
cellSize = 40

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
  | ActionAskPlayerWhite MisoString
  | ActionNewGame

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect parentModel Model Action

updateModel (ActionAskPlayerWhite pt) = do
  modelLog .= pt <> " plays White"
  case pt of
    "Human"   -> modelPlayerWhite .= Human
    "Random"  -> modelPlayerWhite .= BotRandom >> tryPlayBotWhite
    "McEasy"  -> modelPlayerWhite .= BotMcEasy >> tryPlayBotWhite
    "McHard"  -> modelPlayerWhite .= BotMcHard >> tryPlayBotWhite
    _         -> pure ()

updateModel ActionNewGame = do
  modelGame %= Othello.Game.reset
  modelLog .= "new game"
  tryPlayBotWhite

updateModel (ActionAskPlay event) = do
  game <- use modelGame
  when (isRunning game && button event == 0) $ do
    let ij = uncurry xy2ij $ offset event 
    case getCurrentPlayer game of
      PlayerBlack -> do
        doPlay PlayerBlack (Move ij)
        tryPlayBotWhite
      PlayerWhite -> do
        moveWhite <- fromMaybe (Move ij) <$> genMovePlayerWhite
        doPlay PlayerWhite moveWhite

doPlay :: Player -> Move -> Effect parentModel Model Action
doPlay player move@(Move ij) = do
  game <- use modelGame
  case play move game of
    Nothing -> modelLog .= fmtLogPlay player False ij
    Just game' -> do
      modelLog .= fmtLogPlay player True ij
      modelGame .= game'

tryPlayBotWhite :: Effect parentModel Model Action
tryPlayBotWhite = do
  game <- use modelGame
  let player = getCurrentPlayer game
  when (isRunning game && player == PlayerWhite) $ do
    mMoveO <- genMovePlayerWhite
    forM_ mMoveO (doPlay PlayerWhite)

fmtLogPlay :: Player -> Bool -> (Int, Int) -> MisoString
fmtLogPlay p ok ij =
  let pStr = if p == PlayerBlack then "Black" else "White"
      okStr = if ok then "played" else "failed to play"
      ijStr = ms $ show ij
  in pStr <> " " <> okStr <> " " <> ijStr

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View parent Action
viewModel model =
  div_ [] 
    [ p_ [] 
        [ text "player White: "
        , select_ [ onChange ActionAskPlayerWhite ]
            [ option_ [ selected_ (model^.modelPlayerWhite == Human) ]      [ "Human" ]
            , option_ [ selected_ (model^.modelPlayerWhite == BotRandom) ]  [ "Random" ]
            , option_ [ selected_ (model^.modelPlayerWhite == BotMcEasy) ]  [ "McEasy" ]
            , option_ [ selected_ (model^.modelPlayerWhite == BotMcHard) ]  [ "McHard" ]
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
      BlackPlays  -> "Black plays"
      WhitePlays  -> "White plays"
      BlackWins   -> "Black wins"
      WhiteWins   -> "White wins"
      Draw        -> "it's a draw"

initCanvas :: DOMRef -> Canvas ()
initCanvas _ = pure ()

drawCanvas :: Int -> Int -> Double -> Double -> Model -> () -> Canvas ()
drawCanvas ni nj canvasWidthD canvasHeightD model () = do
  Canvas.clearRect (0, 0, canvasWidthD, canvasHeightD)
  let bg = if model^.modelGame & isRunning then bgColor else bgColorEnd
  drawBackground bg canvasWidthD canvasHeightD
  drawGrid CSS.black nj ni cellSize cellSize canvasWidthD canvasHeightD
  forGame (model^.modelGame) (drawGameCell ni)

drawGameCell :: Int -> Int -> Int -> Cell -> Canvas ()
drawGameCell ni i j = \case
  CellBlack -> drawPiece ni CSS.black i j
  CellWhite -> drawPiece ni CSS.white i j
  CellEmpty -> pure ()

drawPiece :: Int -> CSS.Color -> Int -> Int -> Canvas ()
drawPiece ni col i' j = do

  let i = ni - i' - 1

  Canvas.save ()
  Canvas.translate $ ij2xyC i j

  Canvas.beginPath ()
  Canvas.fillStyle (Canvas.color col)
  Canvas.arc (0, 0, cs04, 0, 2*pi)
  Canvas.fill ()

  Canvas.restore ()

cs04 :: Double
cs04 = cellSizeD * 0.4

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: Model -> Component m Model Action
mkComponent model = 
  (component model updateModel viewModel)
    { events = defaultEvents <> pointerEvents
    -- , logLevel = DebugAll
    }



