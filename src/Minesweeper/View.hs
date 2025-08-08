{-# LANGUAGE OverloadedStrings #-}

module Minesweeper.View where

import Control.Monad (when)
import Miso
import Miso.Canvas as Canvas
import Miso.Lens
import Miso.Style qualified as Style

import Helpers.Canvas
import Minesweeper.Game
import Minesweeper.Helpers
import Minesweeper.Model
import Minesweeper.Update

