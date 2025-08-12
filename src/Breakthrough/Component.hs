{-# LANGUAGE OverloadedStrings #-}

module Breakthrough.Component (mkComponent) where

import Control.Monad (forM_, when)
import Miso
import Miso.Lens
import Miso.Canvas (Canvas, canvas)
import Miso.Canvas qualified as Canvas 
import Miso.Style qualified as Style

import Game
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
cellSize = 50

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
  | ActionAskPlayerBlue MisoString
  | ActionNewGame

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect parentModel Model Action

updateModel (ActionAskPlayerBlue pt) = do
  modelSelected .= Nothing
  modelLog .= pt <> " plays Blue"
  case pt of
    "Human"   -> modelPlayerBlue .= Human
    "Random"  -> modelPlayerBlue .= BotRandom >> tryPlayBotBlue
    "McEasy"  -> modelPlayerBlue .= BotMcEasy >> tryPlayBotBlue
    "McHard"  -> modelPlayerBlue .= BotMcHard >> tryPlayBotBlue
    _         -> pure ()

updateModel ActionNewGame = do
  modelGame %= Breakthrough.Game.reset
  modelSelected .= Nothing
  modelLog .= "new game"
  tryPlayBotBlue

updateModel (ActionAskPlay event) = do
  game <- use modelGame
  when (isRunning game && button event == 0) $ do
    selected <- use modelSelected
    let ij@(i, j) = uncurry xy2ij $ offset event 
        ijStr = ms (show i <> " " <> show j)
        player = game & getCurrentPlayer
        playerStr = case player of
          PlayerRed -> "Red"
          PlayerBlue -> "Blue"
    case selected of
      Nothing -> 
        when (ij `elem` getMovesFrom game) $ do
          modelSelected .= Just ij
          modelLog .= playerStr <> " selected " <> ijStr
      Just ij0@(i0, j0) -> do
        modelSelected .= Nothing
        let ij0Str = ms (show i0 <> " " <> show j0)
        if ij `elem` getMovesTo ij0 game
          then do
            doPlay player (Move ij0 ij)
            tryPlayBotBlue
          else 
            if ij `elem` getMovesFrom game
              then do
                modelSelected .= Just ij
                modelLog .= playerStr <> " selected " <> ijStr
              else modelLog .= playerStr <> " deselected " <> ij0Str

tryPlayBotBlue :: Effect parentModel Model Action
tryPlayBotBlue = do
  game <- use modelGame
  let player = getCurrentPlayer game
  when (isRunning game && player == PlayerBlue) $ do
    mMoveBlue <- genMovePlayerBlue
    forM_ mMoveBlue (doPlay PlayerBlue)

doPlay :: Player -> Move -> Effect parentModel Model Action
doPlay player move = do
  game <- use modelGame
  let playerStr = case player of
          PlayerRed -> "Red"
          PlayerBlue -> "Blue"
      ij0Str = move ^. moveFrom & show & ms
      ijStr = move ^. moveTo & show & ms
  case play move game of
    Nothing -> modelLog .= playerStr <> " failed to play " <> ij0Str <> " -> " <> ijStr
    Just game' -> do
      modelLog .= playerStr <> " played " <> ij0Str <> " -> " <> ijStr
      modelGame .= game'

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

cellSizeD :: Double
cellSizeD = fromIntegral cellSize

viewModel :: Model -> View parent Action
viewModel model =
  div_ [] 
    [ p_ [] 
        [ text "player Blue: "
        , select_ [ onChange ActionAskPlayerBlue ]
            [ option_ [ selected_ (model^.modelPlayerBlue == Human) ]      [ "Human" ]
            , option_ [ selected_ (model^.modelPlayerBlue == BotRandom) ]  [ "Random" ]
            , option_ [ selected_ (model^.modelPlayerBlue == BotMcEasy) ]  [ "McEasy" ]
            , option_ [ selected_ (model^.modelPlayerBlue == BotMcHard) ]  [ "McHard" ]
            ]
        ]
    , p_ [] [ button_ [ onClick ActionNewGame ] [ "new game" ] ]
    , canvas 
        [ width_ (ms $ show canvasWidthD)
        , height_ (ms $ show canvasHeightD)
        , Style.style_  [Style.border "2px solid black"]
        , onPointerUp ActionAskPlay
        ]
      initCanvas
      (drawCanvas ni nj canvasWidthD canvasHeightD model)
    , p_ [] 
        [ text ("status: " <> (model^.modelGame & getStatus & fmtStatus))
        , br_ []
        , text ("log: " <> model^.modelLog)
        ]
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
  case model^.modelSelected of
    Nothing -> 
      forM_ (model^.modelGame & getMovesFrom) $ 
        uncurry (drawDisc Style.white cs045)
    Just ij0@(i0, j0) -> do
      drawDisc Style.black cs045 i0 j0
      forM_ (model^.modelGame & getMovesTo ij0) $ \(i1, j1) -> do
        drawDisc Style.white cs045 i1 j1
        drawDisc bgColor cs04 i1 j1

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

mkComponent :: Model -> Component m Model Action
mkComponent model = do
  (component model updateModel viewModel)
    { events = defaultEvents <> pointerEvents
    -- , logLevel = DebugAll
    }


