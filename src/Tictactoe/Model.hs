{-# LANGUAGE OverloadedStrings #-}

module Tictactoe.Model where

import Control.Monad.State.Lazy
import Miso
import Miso.Lens
import Miso.Lens.TH
import System.Random

import Tictactoe.Game

data PlayerType
  = Human
  | Random
  | McEasy
  | McHard
  deriving (Eq)

data Model = Model
  { _modelGame        :: Game
  , _modelLog         :: MisoString
  , _modelPlayerO     :: PlayerType
  , _modelPlayerOGen  :: StdGen
  } deriving (Eq)

makeLenses ''Model

mkModel :: StdGen -> Model
mkModel = Model mkGame "this is Tictactoe" Random  -- TODO Human

genMovePlayerO :: MonadState Model m => Int -> Int -> m Move
genMovePlayerO i j = do
  playerType <- use modelPlayerO
  case playerType of
    Human -> pure (Move i j)
    _ -> head . getMoves <$> use modelGame  -- TODO

