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
mkGame = Game board moves BlackPlays PlayerBlack PlayerBlack
  where
    board = mkBoard
    moves = []    -- TODO

reset :: Game -> Game
reset = id   -- TODO

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
  

play' :: Move -> Game -> Maybe Game
play' _ = Just   -- TODO

