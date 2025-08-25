{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module ConnectFour.Game
  -- TODO
  -- ( Cell(..)
  -- , Game
  -- , Move(..)
  -- , Player(..)
  -- , Status(..)
  -- , forGame
  -- , getNiNj
  -- , getStatus
  -- , mkGame
  -- , reset
  -- ) 
  where

import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U

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
  = RedPlays
  | YellowPlays
  | RedWins
  | YellowWins
  | Draw
  deriving (Eq, Show)

data Player
  = PlayerRed
  | PlayerYellow
  deriving (Eq, Show)

data Cell
  = CellEmpty
  | CellYellow
  | CellRed
  deriving (Eq, Show)

type Board = V.Vector Cell

newtype Move = Move Int
  deriving (Eq, Show)

data Game = Game
  { _gameBoard          :: Board
  , _gameLastJs         :: U.Vector Int
  , _gameStatus         :: Status
  , _gameInitialPlayer  :: Player
  , _gameCurrentPlayer  :: Player
  } deriving (Eq)

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

instance GameClass Game Move Player where
  getPossibleMoves = getPossibleMoves'
  getCurrentPlayer = _gameCurrentPlayer
  isRunning = isRunning'
  play = play'

  scoreForPlayer p Game{..} =
    case (p, _gameStatus) of
      (PlayerRed,     RedWins)      ->  1
      (PlayerRed,     YellowWins)   -> -1
      (PlayerYellow,  YellowWins)   ->  1
      (PlayerYellow,  RedWins)      -> -1
      _                             ->  0

mkGame :: Game
mkGame = Game mkBoard lastJs RedPlays PlayerRed PlayerRed
  where
    lastJs = U.replicate paramNj 0

mkBoard :: Board
mkBoard = V.replicate (paramNi*paramNj) CellEmpty 

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
        PlayerRed     -> (PlayerYellow, YellowPlays)
        PlayerYellow  -> (PlayerRed, RedPlays)

getStatus :: Game -> Status
getStatus = _gameStatus

forGame :: (Monad m) => Game -> (Int -> Int -> Cell -> m ()) -> m ()
forGame Game{..} f = 
  V.iforM_ _gameBoard $ \k c -> 
    let (i, j) = k2ij k
    in f i j c

getNiNj :: (Int, Int)
getNiNj = (paramNi, paramNj)

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

ij2k :: (Int, Int) -> Int
ij2k (i, j) = i*paramNj + j

k2ij :: Int -> (Int, Int)
k2ij k = (k `div` paramNj, k`rem` paramNj)

isRunning' :: Game -> Bool
isRunning' Game{..} = _gameStatus == RedPlays || _gameStatus == YellowPlays

checkWin :: Board -> Cell -> Int -> Int -> Bool
checkWin board cell i0 j0 
  =  countCells (-1) 0 >= 3                       -- vertical
  || countCells 0 1 + countCells 0 (-1) >= 3      -- horizontal
  || countCells 1 1 + countCells (-1) (-1) >= 3   -- diagonal 1
  || countCells (-1) 1 + countCells 1 (-1) >= 3   -- diagonal 2

  where
    countCells :: Int -> Int -> Int
    countCells di dj = go di dj (i0+di) (j0+dj) 0

    go di dj i j acc = 
      if i>=0 && i<paramNi && j>=0 && j<paramNj && board V.! ij2k (i, j) == cell
        then go di dj (i+di) (j+dj) (acc+1)
        else acc

play' :: Move -> Game -> Maybe Game
play' (Move j) g@Game{..} = 
  if not (isRunning' g) || i < paramNi
    then Nothing
    else Just $ Game board lastJs status _gameInitialPlayer nextPlayer
  where
    i = _gameLastJs U.! j
    k = ij2k (i, j)
    (cell, winP, nextP, nextS) = 
      if _gameCurrentPlayer == PlayerRed 
        then (CellRed, RedWins, PlayerYellow, YellowPlays)
        else (CellYellow, YellowWins, PlayerRed, RedPlays)
    board = _gameBoard V.// [(k, cell)]
    lastJs = _gameLastJs U.// [(j, i+1)]
    (status, nextPlayer)
      | checkWin board cell i j = (winP, _gameCurrentPlayer)
      | U.all (>=paramNi) _gameLastJs = (Draw, _gameCurrentPlayer)
      | otherwise = (nextS, nextP)

getPossibleMoves' :: Game -> [Move]
getPossibleMoves' Game{..} = U.ifoldl' f [] _gameLastJs
  where
    f acc j i = if i < paramNi then Move j : acc else acc

