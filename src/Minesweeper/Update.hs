{-# LANGUAGE OverloadedStrings #-}

module Minesweeper.Update where

import Control.Monad.IO.Class (liftIO)
import Miso
import Miso.Lens

import Minesweeper.Game
import Minesweeper.Helpers
import Minesweeper.Model

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

data Action 
  = ActionAskReset Mode
  | ActionAskPlay PointerEvent
  | ActionSetModel Model

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect parentModel Model Action

updateModel (ActionAskReset mode) = do
  model <- get
  io (ActionSetModel <$> liftIO (resetModel mode model))

updateModel (ActionSetModel model) = 
  put model

updateModel (ActionAskPlay event) = do
  let (i, j) = uncurry xy2ij' $ offset event 
  case button event of
    0 -> do
      io_ (consoleLog ("playFree " <> ms (show i) <> " " <> ms (show j)))
      mGame %= play (MoveFree i j)
    1 -> do
      io_ (consoleLog ("playFlag " <> ms (show i) <> " " <> ms (show j)))
      mGame %= play (MoveFlag i j)
    _ -> pure ()

