{-# LANGUAGE OverloadedStrings #-}

module Minesweeper.Component (mkComponent) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Miso
import Miso.Canvas as Canvas
import Miso.Lens
import Miso.CSS qualified as CSS
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P

import Helpers.Canvas
import Minesweeper.Game
import Minesweeper.Model

-------------------------------------------------------------------------------
-- view params
-------------------------------------------------------------------------------

cellSize :: Int
cellSize = 25

cellFont :: MisoString
cellFont = "small-caps bold 18px arial"

colorNo, colorYes, colorWrongFlag, colorWrongMine :: CSS.Color
colorNo = CSS.hex 0xBBBBBB
colorYes = CSS.hex 0xDDDDDD
colorWrongFlag = CSS.hex 0x88DD88
colorWrongMine = CSS.hex 0xDD8888

n2color :: Int -> CSS.Color
n2color = \case
  1 -> CSS.hex 0x0000FF
  2 -> CSS.hex 0x007B00
  3 -> CSS.hex 0xFF0000
  4 -> CSS.hex 0x00007B
  5 -> CSS.hex 0x7B0000
  _ -> CSS.black

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

ij2xy :: Int -> Int -> (Double, Double)
ij2xy = ij2xyTL' cellSize cellSize

xy2ij :: Double -> Double -> (Int, Int)
xy2ij = xy2ij' cellSize cellSize

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

data Action 
  = ActionAskReset Mode
  | ActionAskPlay PointerEvent
  | ActionSetModel Model
  | ActionNone

updateModel :: Action -> Effect parentModel Model Action

updateModel (ActionAskReset mode) = do
  model <- get
  io (ActionSetModel <$> liftIO (resetModel mode model))

updateModel (ActionSetModel model) = 
  put model

updateModel (ActionAskPlay event) = do
  running <- isRunning <$> use modelGame
  when running $ do
    let (i, j) = uncurry xy2ij $ offset event 
    case button event of
      0 -> do
        modelLog .= "playFree " <> ms (show i) <> " " <> ms (show j)
        modelGame %= play (MoveFree i j)
      2 -> do
        modelLog .= "playFlag " <> ms (show i) <> " " <> ms (show j)
        modelGame %= play (MoveFlag i j)
      _ -> pure ()

updateModel ActionNone = pure ()

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

cellSizeD :: Double
cellSizeD = fromIntegral cellSize

cs007, cs01, cs02, cs03, cs04, cs05, cs06, cs08, cs09 :: Double
cs007 = cellSizeD * 0.07
cs01 = cellSizeD * 0.1
cs02 = cellSizeD * 0.2
cs03 = cellSizeD * 0.3
cs04 = cellSizeD * 0.4
cs05 = cellSizeD * 0.5
cs06 = cellSizeD * 0.6
cs08 = cellSizeD * 0.8
cs09 = cellSizeD * 0.9

viewModel :: Model -> View Model Action
viewModel model = div_ [] 
  [ p_ [] [ "left-click to discover, right-click to flag/unflag" ]
  , p_ [] 
      [ button_ [ onClick (ActionAskReset ModeBeginner) ]     [ "beginner" ]
      , button_ [ onClick (ActionAskReset ModeIntermediate) ] [ "intermediate" ]
      , button_ [ onClick (ActionAskReset ModeExpert) ]       [ "expert" ]
      ]
  , Canvas.canvas 
      [ width_ (ms $ nj * cellSize)
      , height_ (ms $ ni * cellSize)
      , CSS.style_  [CSS.border "2px solid black"]
      , onPointerUp ActionAskPlay
      , onContextMenuWithOptions ActionNone (defaultOptions { preventDefault = True })
      ]
    initCanvas
    (drawCanvas model)
  , p_ [] 
        [ text ("status: " <> fmtStatus (model ^. modelGame & getStatus))
        , br_ []
        , text ("flags: " <> ms (show $ model ^. modelGame & getFlags))
        , br_ []
        , text ("mines: " <> ms (show $ model ^. modelGame & getNbMines))
        , br_ []
        , text ("remaining cells: " <> ms (show $ model ^. modelGame & getRemCells))
        , br_ []
        , text ("log: " <> model^.modelLog)
        ]
  ]
  where
    (ni, nj) = model ^. modelGame & getBoardNiNj

    fmtStatus = \case
      StatusRunning   -> "running"
      StatusWon       -> "won"
      StatusLost      -> "lost"

initCanvas :: DOMRef -> Canvas ()
initCanvas _ = pure ()

drawCanvas :: Model -> () -> Canvas ()
drawCanvas model () = do
  let (ni, nj) = model ^. modelGame & getBoardNiNj
      w = fromIntegral $ nj * cellSize
      h = fromIntegral $ ni * cellSize
  clearRect (0, 0, w, h)
  font cellFont
  drawBackground colorNo w h
  forGame (model ^. modelGame) drawGameCell
  drawGrid CSS.black ni nj cellSize cellSize w h

drawCell :: CSS.Color -> Canvas ()
drawCell c = do
  fillStyle (color c)
  fillRect (0, 0, cellSizeD, cellSizeD)

drawMine :: Bool -> Int -> Int -> Canvas ()
drawMine wrong i j = do

  save ()
  translate $ ij2xy i j

  when wrong $ drawCell colorWrongMine

  beginPath ()
  moveTo (cs02, cs02)
  lineTo (cs08, cs08)
  moveTo (cs02, cs08)
  lineTo (cs08, cs02)
  moveTo (cs05, cs01)
  lineTo (cs05, cs09)
  moveTo (cs01, cs05)
  lineTo (cs09, cs05)
  stroke ()

  fillStyle (color CSS.black)
  beginPath ()
  arc (cs05, cs05, cs03, 0, 2*pi)
  fill ()

  fillStyle (color CSS.white)
  beginPath ()
  arc (cs04, cs04, cs007, 0, 2*pi)
  fill ()

  restore ()

drawFlag :: Bool -> Int -> Int -> Canvas ()
drawFlag wrong i j = do

  save ()
  translate $ ij2xy i j

  when wrong $ drawCell colorWrongFlag

  fillStyle (color CSS.red)
  beginPath ()
  moveTo (cs02, cs04)
  lineTo (cs06, cs02)
  lineTo (cs06, cs06)
  closePath ()
  fill ()

  fillStyle (color CSS.black)
  fillRect (cs06, cs02, cs01, cs06)

  restore ()

drawFree :: Int -> Int -> Int -> Canvas ()
drawFree i j n = do
  save ()
  translate $ ij2xy i j
  drawCell colorYes
  fillStyle (color $ n2color n)
  when (n > 0) $ fillText (ms (show n), cs03, cs08)
  restore ()

drawGameCell :: Int -> Int -> Cell -> Canvas ()
drawGameCell i j = \case
  CellUnknown -> pure ()
  CellFree n  -> drawFree i j n
  CellFlag    -> drawFlag False i j
  CellFlagKo  -> drawFlag True i j
  CellMine    -> drawMine False i j
  CellMineKo  -> drawMine True i j

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: Model -> Component m Model Action
mkComponent model =
  (component model updateModel viewModel)
    { events = defaultEvents <> pointerEvents
    -- , logLevel = DebugAll
    }

