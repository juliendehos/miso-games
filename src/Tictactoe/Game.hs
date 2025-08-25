{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Tictactoe.Game
  ( Cell(..)
  , Game
  , Move(..)
  , Player(..)
  , Status(..)
  , forGame
  , getNiNj
  , getStatus
  , mkGame
  , reset
  ) where

import Data.Vector qualified as V

import Game

-------------------------------------------------------------------------------
-- types
-------------------------------------------------------------------------------

data Status
  = XPlays
  | OPlays
  | XWins
  | OWins
  | Draw
  deriving (Eq, Show)

data Player
  = PlayerX
  | PlayerO
  deriving (Eq, Show)

data Cell
  = CellEmpty
  | CellX
  | CellO
  deriving (Eq, Show)

type Board = V.Vector Cell

data Move = Move Int Int
  deriving (Eq, Show)

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

instance GameClass Game Move Player where
  getPossibleMoves = _gameMoves
  getCurrentPlayer = _gameCurrentPlayer
  isRunning = isRunning'
  play = play'

  scoreForPlayer p Game{..} =
    case (p, _gameStatus) of
      (PlayerX, XWins)  ->  1
      (PlayerX, OWins)  -> -1
      (PlayerO, OWins)  ->  1
      (PlayerO, XWins)  -> -1
      _                 ->  0

mkGame :: Game
mkGame = Game board 9 moves XPlays PlayerX PlayerX
  where
    board = V.replicate 9 CellEmpty 
    moves = [ Move i j | i<-[0..2], j<-[0..2] ]

reset :: Game -> Game
reset g0 = 
  mkGame 
    { _gameStatus = status
    , _gameCurrentPlayer = player
    , _gameInitialPlayer = player
    }
  where
    (player, status) = 
      case _gameInitialPlayer g0 of
        PlayerX -> (PlayerO, OPlays)
        PlayerO -> (PlayerX, XPlays)

getStatus :: Game -> Status
getStatus = _gameStatus

forGame :: (Monad m) => Game -> (Int -> Int -> Cell -> m ()) -> m ()
forGame Game{..} f = 
  V.iforM_ _gameBoard $ \k c -> 
    let (i, j) = k2ij k
    in f i j c

getNiNj :: (Int, Int)
getNiNj = (3, 3)

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

ij2k :: (Int, Int) -> Int
ij2k (i, j) = i*3 + j

k2ij :: Int -> (Int, Int)
k2ij k = (k `div` 3, k`rem` 3)

isRunning' :: Game -> Bool
isRunning' Game{..} = _gameStatus == XPlays || _gameStatus == OPlays

play' :: Move -> Game -> Maybe Game
play' m@(Move i j) g@Game{..} =
  if not (isRunning' g) ||_gameBoard V.! ij2k (i, j) /= CellEmpty
    then Nothing
    else 
      Just $ Game board remMoves moves status _gameInitialPlayer nextPlayer
  where
    (cell, nextPlayer) = case _gameCurrentPlayer of
      PlayerX -> (CellX, PlayerO)
      PlayerO -> (CellO, PlayerX)
    board = _gameBoard V.// [(ij2k (i, j), cell)]
    remMoves = _gameRemMoves - 1
    moves = filter (/=m) _gameMoves
    status = computeStatus m cell board remMoves

computeStatus :: Move -> Cell -> Board -> Int -> Status
computeStatus (Move i j) cell b remMoves =
  case (win, cell, remMoves) of
    (True, CellX, _)  -> XWins
    (True, CellO, _)  -> OWins
    (False, _, 0)     -> Draw
    (False, CellX, _) -> OPlays
    _                 -> XPlays
  where
    f i' j' = b V.! ij2k (i', j') == cell
    win = f i 0 && f i 1 && f i 2 ||
          f 0 j && f 1 j && f 2 j ||
          f 0 0 && f 1 1 && f 2 2 ||
          f 0 2 && f 1 1 && f 2 0

