{-# LANGUAGE RecordWildCards #-}

module Tictactoe.Game
  ( Game
  , Status(..)
  , Player(..)
  , mkGame
  , reset
  , play
  , getPlayers
  , getMoves
  , getStatus
  , getCurrentPlayer
  , getCellsPlayer1
  , getCellsPlayer2
  , isRunning
  ) where

import Data.Vector as V hiding (filter)

import Game

-------------------------------------------------------------------------------
-- types
-------------------------------------------------------------------------------

data Cell
  = CellEmpty
  | CellPlayer1
  | CellPlayer2
  deriving (Eq)

type Board = Vector Cell
type Move = (Int, Int)

data Game = Game
  { _gameBoard          :: Board
  , _gameRemMoves       :: Int
  , _gameMoves          :: [Move]
  , _gameStatus         :: Status
  , _gameInitialPlayer  :: Player
  , _gameCurrentPlayer  :: Player
  } deriving (Eq)

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

play :: Move -> Game -> Game
play ij g@Game{..} =
  if _gameBoard ! k /= CellEmpty
    then g
    else 
      Game b rm ms s _gameInitialPlayer np
  where
    k = ij2k ij
    (c, np) = case _gameCurrentPlayer of
      Player1 -> (CellPlayer1, Player2)
      Player2 -> (CellPlayer2, Player1)
    b = _gameBoard // [(k, c)]
    rm = _gameRemMoves - 1
    ms = filter (/=ij) _gameMoves
    s = computeStatus ij c b rm

mkGame :: Game
mkGame = Game board 9 moves Player1Plays Player1 Player1
  where
    board = V.replicate 9 CellEmpty 
    moves = [ (i, j) | i<-[0..2], j<-[0..2] ]

reset :: Game -> Game
reset g0 = 
  mkGame 
    { _gameStatus = status
    , _gameInitialPlayer = player
    }
  where
    (player, status) = 
      case _gameInitialPlayer g0 of
        Player1 -> (Player2, Player2Plays)
        Player2 -> (Player1, Player1Plays)

getPlayers :: [Player]
getPlayers = [Player1, Player2]

getMoves :: Game -> [Move]
getMoves = _gameMoves

getStatus :: Game -> Status
getStatus = _gameStatus

getCurrentPlayer :: Game -> Player
getCurrentPlayer = _gameCurrentPlayer

getCellsPlayer1 :: Game -> [(Int, Int)]
getCellsPlayer1 = ifoldl f [] . _gameBoard
  where
    f acc k c = if c == CellPlayer1 then k2ij k : acc else acc

getCellsPlayer2 :: Game -> [(Int, Int)]
getCellsPlayer2 = ifoldl f [] . _gameBoard
  where
    f acc k c = if c == CellPlayer2 then k2ij k : acc else acc

isRunning :: Game -> Bool
isRunning Game{..} = _gameStatus == Player1Plays || _gameStatus == Player2Plays

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

ij2k :: (Int, Int) -> Int
ij2k (i, j) = i*3 + j

k2ij :: Int -> (Int, Int)
k2ij k = (div k 3, mod k 3)

computeStatus :: Move -> Cell -> Board -> Int -> Status
computeStatus (i, j) c b rm =
  case (win, c, rm) of
    (True, CellPlayer1, _)  -> Player1Wins
    (True, CellPlayer2, _)  -> Player2Wins
    (False, _, 0)           -> Draw
    (False, CellPlayer1, _) -> Player2Plays
    _                       -> Player1Plays
  where
    f ij' = b ! ij2k ij' == c
    win = f (i, 0) && f (i, 1) && f (i, 2) ||
          f (0, j) && f (1, j) && f (2, j) ||
          f (0, 0) && f (1, 1) && f (2, 2) ||
          f (0, 2) && f (1, 1) && f (2, 0)

