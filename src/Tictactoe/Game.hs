{-# LANGUAGE RecordWildCards #-}

module Tictactoe.Game
  ( Game
  , Status(..)
  , Player(..)
  , Cell(..)
  , Move(..)
  , mkGame
  , reset
  , play
  , getPlayers
  , getMoves
  , getStatus
  , getCurrentPlayer
  , forGame
  , isRunning
  ) where

import Data.Vector as V hiding (filter)

-------------------------------------------------------------------------------
-- types
-------------------------------------------------------------------------------

data Status
  = XPlays
  | OPlays
  | XWins
  | OWins
  | Draw
  deriving (Eq)

data Player
  = PlayerX
  | PlayerO
  deriving (Eq)

data Cell
  = CellEmpty
  | CellX
  | CellO
  deriving (Eq)

type Board = Vector Cell

data Move = Move Int Int
  deriving (Eq)

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

play :: Move -> Game -> Maybe Game
play m g@Game{..} =
  if not (isRunning g) || _gameBoard ! k /= CellEmpty
    then Nothing
    else 
      Just $ Game b rm ms s _gameInitialPlayer np
  where
    k = move2k m
    (c, np) = case _gameCurrentPlayer of
      PlayerX -> (CellX, PlayerO)
      PlayerO -> (CellO, PlayerX)
    b = _gameBoard // [(k, c)]
    rm = _gameRemMoves - 1
    ms = filter (/=m) _gameMoves
    s = computeStatus m c b rm

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

getPlayers :: [Player]
getPlayers = [PlayerX, PlayerO]

getMoves :: Game -> [Move]
getMoves = _gameMoves

getStatus :: Game -> Status
getStatus = _gameStatus

getCurrentPlayer :: Game -> Player
getCurrentPlayer = _gameCurrentPlayer

isRunning :: Game -> Bool
isRunning Game{..} = _gameStatus == XPlays || _gameStatus == OPlays

forGame :: (Monad m) => Game -> (Int -> Int -> Cell -> m ()) -> m ()
forGame game f = V.iforM_ (_gameBoard game) $ \k c -> 
    let (i, j) = k2ij k
    in f i j c

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

move2k :: Move -> Int
move2k (Move i j) = i*3 + j

ij2k :: (Int, Int) -> Int
ij2k (i, j) = i*3 + j

k2ij :: Int -> (Int, Int)
k2ij k = (div k 3, mod k 3)

computeStatus :: Move -> Cell -> Board -> Int -> Status
computeStatus (Move i j) c b rm =
  case (win, c, rm) of
    (True, CellX, _)  -> XWins
    (True, CellO, _)  -> OWins
    (False, _, 0)     -> Draw
    (False, CellX, _) -> OPlays
    _                 -> XPlays
  where
    f ij' = b ! ij2k ij' == c
    win = f (i, 0) && f (i, 1) && f (i, 2) ||
          f (0, j) && f (1, j) && f (2, j) ||
          f (0, 0) && f (1, 1) && f (2, 2) ||
          f (0, 2) && f (1, 1) && f (2, 0)

