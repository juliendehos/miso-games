{-# LANGUAGE RecordWildCards #-}

module Tictactoe.Game
  ( Cell(..)
  , Game
  , Move(..)
  , Player(..)
  , Status(..)
  , forGame
  , getPlayers
  , getMoves
  , getStatus
  , getCurrentPlayer
  , isRunning
  , mkGame
  , play
  , reset
  ) where

import Helpers.Board

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

type Board = Board' Cell

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
play m@(Move i j) g@Game{..} =
  if not (isRunning g) || getIJ _gameBoard i j /= CellEmpty
    then Nothing
    else 
      Just $ Game b rm ms s _gameInitialPlayer np
  where
    (c, np) = case _gameCurrentPlayer of
      PlayerX -> (CellX, PlayerO)
      PlayerO -> (CellO, PlayerX)
    b = setIJ _gameBoard i j c
    rm = _gameRemMoves - 1
    ms = filter (/=m) _gameMoves
    s = computeStatus m c b rm

mkGame :: Game
mkGame = Game board 9 moves XPlays PlayerX PlayerX
  where
    board = mkBoardFromVal 3 3 CellEmpty 
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
forGame Game{..} = forBoard _gameBoard

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

computeStatus :: Move -> Cell -> Board -> Int -> Status
computeStatus (Move i j) c b rm =
  case (win, c, rm) of
    (True, CellX, _)  -> XWins
    (True, CellO, _)  -> OWins
    (False, _, 0)     -> Draw
    (False, CellX, _) -> OPlays
    _                 -> XPlays
  where
    f i' j' = getIJ b i' j' == c
    win = f i 0 && f i 1 && f i 2 ||
          f 0 j && f 1 j && f 2 j ||
          f 0 0 && f 1 1 && f 2 2 ||
          f 0 2 && f 1 1 && f 2 0

