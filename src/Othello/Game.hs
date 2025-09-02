{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Othello.Game
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
-- params
-------------------------------------------------------------------------------

paramNi, paramNj :: Int
paramNi = 8
paramNj = 8

-------------------------------------------------------------------------------
-- types
-------------------------------------------------------------------------------

data Status
  = BlackPlays
  | WhitePlays
  | BlackWins
  | WhiteWins
  | Draw
  deriving (Eq, Show)

data Player
  = PlayerBlack
  | PlayerWhite
  deriving (Eq, Show)

data Cell
  = CellEmpty
  | CellWhite
  | CellBlack
  deriving (Eq, Show)

type Board = V.Vector Cell

newtype Move = Move (Int, Int)
  deriving (Eq, Show)

data Game = Game
  { _gameBoard          :: Board
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
  scoreForPlayer = scoreForPlayer'

mkGame :: Game
mkGame = mkGame' PlayerBlack

reset :: Game -> Game
reset Game{..} = case _gameInitialPlayer of
  PlayerBlack -> mkGame' PlayerWhite
  PlayerWhite -> mkGame' PlayerBlack

getStatus :: Game -> Status
getStatus = _gameStatus

getNiNj :: (Int, Int)
getNiNj = (paramNi, paramNj)

forGame :: (Monad m) => Game -> (Int -> Int -> Cell -> m ()) -> m ()
forGame Game{..} f = 
  V.iforM_ _gameBoard $ \k c -> 
    let (i, j) = k2ij k
    in f i j c

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

ij2k :: (Int, Int) -> Int
ij2k (i, j) = i*paramNj + j

k2ij :: Int -> (Int, Int)
k2ij k = (k `div` paramNj, k`rem` paramNj)

isRunning' :: Game -> Bool
isRunning' Game{..} = _gameStatus == BlackPlays || _gameStatus == WhitePlays

scoreForPlayer' :: Player -> Game -> Int
scoreForPlayer' p Game{..} =
  case (p, _gameStatus) of
    (PlayerBlack,  BlackWins) ->  1
    (PlayerBlack,  WhiteWins) -> -1
    (PlayerWhite,  WhiteWins) ->  1
    (PlayerWhite,  BlackWins) -> -1
    _                         ->  0

mkBoard :: Board
mkBoard = 
  V.replicate (paramNi*paramNj) CellEmpty V.// 
    [ (ij2k (3, 3), CellWhite)
    , (ij2k (3, 4), CellBlack)
    , (ij2k (4, 3), CellBlack)
    , (ij2k (4, 4), CellWhite)
    ]

mkGame' :: Player -> Game
mkGame' player = Game board moves status player player
  where
    status = case player of
      PlayerBlack -> BlackPlays
      PlayerWhite -> WhitePlays
    board = mkBoard
    moves = computeMoves board player

computeMoves :: Board -> Player -> [Move]
computeMoves board player = V.ifoldl' f [] board
  where
    (cell, cellOpp) = 
      case player of
        PlayerBlack -> (CellBlack, CellWhite)
        PlayerWhite -> (CellWhite, CellBlack)

    f acc k c = 
      let ij = k2ij k
      in if c == cell && isMove board cellOpp ij then Move ij : acc else acc

isMove :: Board -> Cell -> (Int, Int) -> Bool
isMove board cellOpp ij = any (isLine board cellOpp ij)
  [ (-1, -1), ( 1,  1)   -- diag 1
  , (-1,  1), ( 1, -1)   -- diag 2
  , ( 0, -1), ( 0,  1)   -- row
  , (-1,  0), ( 1,  0)   -- col
  ] 

isLine :: Board -> Cell -> (Int, Int) -> (Int, Int) -> Bool
isLine board cellOpp (i0, j0) (di, dj) =
  let ij1@(i1, j1) = (i0+di, j0+dj)
  in board V.! ij2k ij1 == cellOpp && go (i1+di, j1+dj)
  where
    go ij@(i, j) = 
      let c = board V.! ij2k ij
      in i>=0 && i<paramNi && j>=0 && j<paramNj && (c == CellEmpty || c == cellOpp && go (i+di, j+dj))

play' :: Move -> Game -> Maybe Game
play' _ = Just   -- TODO

