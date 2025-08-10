
module Breakthrough.Game 
  ( Cell(..)
  , Game
  , Move(..)
  , Player(..)
  , Status(..)
  , forGame
  , getCurrentPlayer
  , getMoves
  , getMovesFrom
  , getMovesTo
  , getNiNj
  , getStatus
  , isRunning
  , mkGame
  , play
  , reset
  ) where

import Data.List (nub)
import Miso.Lens
import Miso.Lens.TH

import Helpers.Board

-------------------------------------------------------------------------------
-- types
-------------------------------------------------------------------------------

data Move = Move
  { _moveFrom :: (Int, Int)
  , _moveTo   :: (Int, Int)
  } deriving (Eq, Show)

makeLenses ''Move

data Status
  = RedPlays
  | BluePlays
  | RedWins
  | BlueWins
  deriving (Eq, Show)

data Cell
  = CellEmpty
  | CellRed
  | CellBlue
  deriving (Eq, Show)

data Player
  = PlayerRed
  | PlayerBlue
  deriving (Eq, Show)

type Board = Board' Cell

makeLenses ''Board

data Game = Game
  { _gameBoard          :: Board
  , _gameStatus         :: Status
  , _gameMoves          :: [Move]   -- possible moves
  , _gameInitialPlayer  :: Player
  , _gameCurrentPlayer  :: Player
  } deriving (Eq)

makeLenses ''Game

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

mkGame :: Int -> Int -> Game
mkGame ni nj = computeGame ni nj PlayerRed

reset :: Game -> Game
reset g = 
  computeGame 
    (g^.gameBoard^.boardNi)
    (g^.gameBoard^.boardNj) 
    (if g ^. gameInitialPlayer == PlayerRed then PlayerBlue else PlayerRed)

isRunning :: Game -> Bool
isRunning g = g^.gameStatus == RedPlays || g^.gameStatus == BluePlays

play :: Move -> Game -> Maybe Game
play m g =
  if m `elem` g^.gameMoves
    then
      let
        -- apply move
        cell = g^.gameCurrentPlayer & player2cell
        board' = g^.gameBoard & setIJs [(m^.moveFrom, CellEmpty), (m^.moveTo, cell)] 
        -- update status and current player
        (status', player') =
          case (isWinning m g, g^.gameCurrentPlayer) of
            (True, PlayerRed)   -> (RedWins, PlayerRed)
            (True, PlayerBlue)  -> (BlueWins, PlayerBlue)
            (False, PlayerRed)  -> (BluePlays, PlayerBlue)
            (False, PlayerBlue) -> (RedPlays, PlayerRed)
        -- update moves
        moves' = computeMoves board' player'
      in Just $ Game board' status' moves' (g^.gameInitialPlayer) player'
    else Nothing

getStatus :: Game -> Status
getStatus = _gameStatus

getMoves :: Game -> [Move]
getMoves = _gameMoves

getCurrentPlayer :: Game -> Player
getCurrentPlayer = _gameCurrentPlayer

getMovesFrom :: Game -> [(Int, Int)]
getMovesFrom g =
  g^.gameMoves 
    & map _moveFrom 
    & nub

getMovesTo :: (Int, Int) -> Game -> [(Int, Int)]
getMovesTo from g =
  g^.gameMoves 
    & filter ((==from) . _moveFrom)
    & map _moveTo

getNiNj :: Game -> (Int, Int)
getNiNj g = (g^.gameBoard^.boardNi, g^.gameBoard^.boardNj)

forGame :: (Monad m) => Game -> (Int -> Int -> Cell -> m ()) -> m ()
forGame g = forBoard (g^.gameBoard)

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

mkBoard :: Int -> Int -> Board
mkBoard ni nj =
  mkBoardFromList ni nj $
    replicate (2*nj) CellBlue <>
    replicate ((ni-4)*nj) CellEmpty <>
    replicate (2*nj) CellRed

computeGame :: Int -> Int -> Player -> Game
computeGame ni nj p = Game board status moves p p
  where
    board = mkBoard ni nj
    moves = computeMoves board p
    status = if p == PlayerRed then RedPlays else BluePlays

computeMoves :: Board -> Player -> [Move]
computeMoves b = \case
  PlayerRed -> go CellRed CellBlue (-1)
  PlayerBlue -> go CellBlue CellRed 1
  where
    ni = b^.boardNi
    nj = b^.boardNj
    go cell1 cell2 deltaI =
      [ Move (i0, j0) (i1, j1) 
      | i0<-[0 .. ni-1]
      , let i1 = i0 + deltaI
      , j0<-[0 .. nj-1]
      , j1<-[j0-1 .. j0+1]
      , i1>=0 && i1<ni && j1>=0 && j1<nj      -- ij1 is inside the board
      , getIJ i0 j0 b == cell1                -- ij0 is a current player's cell
      , let c1 = getIJ i1 j1 b
      , c1==cell2 && j1/=j0 || c1==CellEmpty  -- move to an empty cell or capture (diagonaly) an opponent's cell
      ]

player2cell :: Player -> Cell
player2cell = \case
  PlayerRed -> CellRed
  PlayerBlue -> CellBlue

isWinning :: Move -> Game -> Bool
isWinning m g =
  case g ^. gameCurrentPlayer of
    PlayerRed -> (m^.moveTo & snd) == 0
    PlayerBlue -> (m^.moveTo & snd) == (g^.gameBoard^.boardNj - 1)

