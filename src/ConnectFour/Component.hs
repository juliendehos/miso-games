{-# LANGUAGE OverloadedStrings #-}

module ConnectFour.Component (mkComponent) where

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
import ConnectFour.Game
import ConnectFour.Model

-------------------------------------------------------------------------------
-- view params
-------------------------------------------------------------------------------

bgColor, bgColorEnd :: CSS.Color
bgColor = CSS.hex 0x8888DD
bgColorEnd = CSS.hex 0xDDDDDD

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
  | ActionAskPlayerYellow MisoString
  | ActionNewGame

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect parentModel Model Action

updateModel (ActionAskPlayerYellow pt) = do
  modelLog .= pt <> " plays Yellow"
  case pt of
    "Human"   -> modelPlayerYellow .= Human
    "Random"  -> modelPlayerYellow .= BotRandom >> tryPlayBotYellow
    "McEasy"  -> modelPlayerYellow .= BotMcEasy >> tryPlayBotYellow
    "McHard"  -> modelPlayerYellow .= BotMcHard >> tryPlayBotYellow
    _         -> pure ()

updateModel ActionNewGame = do
  modelGame %= ConnectFour.Game.reset
  modelLog .= "new game"
  tryPlayBotYellow

updateModel (ActionAskPlay event) = do
  game <- use modelGame
  when (isRunning game && button event == 0) $ do
    let (_, j) = uncurry xy2ij $ offset event 
    case getCurrentPlayer game of
      PlayerRed -> do
        doPlay PlayerRed (Move j)
        tryPlayBotYellow
      PlayerYellow -> do
        moveYellow <- fromMaybe (Move j) <$> genMovePlayerYellow
        doPlay PlayerYellow moveYellow

doPlay :: Player -> Move -> Effect parentModel Model Action
doPlay player move@(Move j) = do
  game <- use modelGame
  case play move game of
    Nothing -> modelLog .= fmtLogPlay player False j
    Just game' -> do
      modelLog .= fmtLogPlay player True j
      modelGame .= game'

tryPlayBotYellow :: Effect parentModel Model Action
tryPlayBotYellow = do
  game <- use modelGame
  let player = getCurrentPlayer game
  when (isRunning game && player == PlayerYellow) $ do
    mMoveO <- genMovePlayerYellow
    forM_ mMoveO (doPlay PlayerYellow)

fmtLogPlay :: Player -> Bool -> Int -> MisoString
fmtLogPlay p ok j =
  let pStr = if p == PlayerRed then "Red" else "Yellow"
      okStr = if ok then "played" else "failed to play"
      jStr = ms $ show j
  in pStr <> " " <> okStr <> " " <> jStr

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View parent Action
viewModel model =
  div_ [] 
    [ p_ [] 
        [ text "player Yellow: "
        , select_ [ onChange ActionAskPlayerYellow ]
            [ option_ [ selected_ (model^.modelPlayerYellow == Human) ]      [ "Human" ]
            , option_ [ selected_ (model^.modelPlayerYellow == BotRandom) ]  [ "Random" ]
            , option_ [ selected_ (model^.modelPlayerYellow == BotMcEasy) ]  [ "McEasy" ]
            , option_ [ selected_ (model^.modelPlayerYellow == BotMcHard) ]  [ "McHard" ]
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
      RedPlays    -> "Red plays"
      YellowPlays -> "Yellow plays"
      RedWins     -> "Red wins"
      YellowWins  -> "Yellow wins"
      Draw        -> "it's a draw"

initCanvas :: DOMRef -> Canvas ()
initCanvas _ = pure ()

drawCanvas :: Int -> Int -> Double -> Double -> Model -> () -> Canvas ()
drawCanvas ni nj canvasWidthD canvasHeightD model () = do
  Canvas.clearRect (0, 0, canvasWidthD, canvasHeightD)
  let bg = if model^.modelGame & isRunning then bgColor else bgColorEnd
  drawBackground bg canvasWidthD canvasHeightD
  drawGrid CSS.black nj ni cellSize cellSize canvasWidthD canvasHeightD
  forGame (model^.modelGame) drawGameCell

drawGameCell :: Int -> Int -> Cell -> Canvas ()
drawGameCell i j = \case
  CellRed -> drawPiece CSS.red i j
  CellYellow -> drawPiece CSS.yellow i j
  CellEmpty -> pure ()

drawPiece :: CSS.Color -> Int -> Int -> Canvas ()
drawPiece col i j = do

  Canvas.save ()
  Canvas.translate $ ij2xyC i j

  Canvas.beginPath ()
  Canvas.fillStyle (Canvas.color col)
  Canvas.arc (0, 0, cs03, 0, 2*pi)
  Canvas.fill ()

  Canvas.restore ()

-- TODO
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


