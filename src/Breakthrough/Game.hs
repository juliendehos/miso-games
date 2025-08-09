{-# LANGUAGE RecordWildCards #-}

module Breakthrough.Game 
  ( Game
  , Move(..)
  , Status(..)
  , getStatus
  , isRunning
  , mkGame
  , play
  ) where

import Miso
import Miso.Lens
import Miso.Lens.TH

import Helpers.Board

-------------------------------------------------------------------------------
-- types
-------------------------------------------------------------------------------

data Move = Move
  { _moveFrom :: (Int, Int)
  , _moveTo   :: (Int, Int)
  } deriving (Eq)

makeLenses ''Move

data Status
  = RedPlays
  | BluePlays
  | RedWins
  | BlueWins
  deriving (Eq)

data Cell
  = CellEmpty
  | CellRed
  | CellBlue
  deriving (Eq)

type Board = Board' Cell

makeLenses ''Board

data Game = Game
  { _gameBoard  :: Board
  , _gameStatus :: Status
  , _gameMoves  :: [Move]   -- possible moves
  } deriving (Eq)

makeLenses ''Game

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

mkGame :: Int -> Int -> Game
mkGame ni nj = Game board RedPlays moves
  where
    board = mkBoardFromList ni nj $
      replicate (2*nj) CellBlue <>
      replicate ((ni-4)*nj) CellEmpty <>
      replicate (2*nj) CellRed
    moves = computeMoves board

isRunning :: Game -> Bool
isRunning Game{..} = _gameStatus == RedPlays || _gameStatus == BluePlays

play :: Move -> Game -> Game
play m g = g    -- TODO

getStatus :: Game -> Status
getStatus = _gameStatus

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

computeMoves :: Board -> [Move]
computeMoves b = []   -- TODO

