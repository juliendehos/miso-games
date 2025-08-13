{-# LANGUAGE RecordWildCards #-}

module Breakthrough.Game 
  ( Cell(..)
  , Game
  , Move(..)
  , Player(..)
  , Status(..)
  , forGame
  , getMovesFrom
  , getMovesTo
  , getNiNj
  , getStatus
  , mkGame
  , moveFrom
  , moveTo
  , reset
  ) where

import Data.List (nub)
import Data.Vector qualified as V
import Miso.Lens
import Miso.Lens.TH

import Game

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

type Board = V.Vector Cell

data Game = Game
  { _gameBoard          :: Board
  , _gameNi             :: Int
  , _gameNj             :: Int
  , _gameStatus         :: Status
  , _gameMoves          :: [Move]   -- possible moves
  , _gameInitialPlayer  :: Player
  , _gameCurrentPlayer  :: Player
  } deriving (Eq)

makeLenses ''Game

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

instance GameClass Game Move Player where
  getPossibleMoves = _gameMoves
  getCurrentPlayer = _gameCurrentPlayer
  isRunning = isRunning'
  play = play'

  scoreForPlayer p g =
    case (p, g^.gameStatus) of
      (PlayerRed,  RedWins)   ->  1
      (PlayerRed,  BlueWins)  -> -1
      (PlayerBlue, BlueWins)  ->  1
      (PlayerBlue, RedWins)   -> -1
      _                       ->  0


mkGame :: Int -> Int -> Game
mkGame ni nj = computeGame ni nj PlayerRed

reset :: Game -> Game
reset g = 
  computeGame 
    (g^.gameNi)
    (g^.gameNj)
    (if g^.gameInitialPlayer == PlayerRed then PlayerBlue else PlayerRed)

getStatus :: Game -> Status
getStatus = _gameStatus

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
getNiNj g = (g^.gameNi, g^.gameNj)

forGame :: (Monad m) => Game -> (Int -> Int -> Cell -> m ()) -> m ()
forGame Game{..} f = 
  V.iforM_ _gameBoard $ \k c -> 
    let (i, j) = k2ij _gameNj k
    in f i j c

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

ij2k :: Int -> (Int, Int) -> Int
ij2k nj (i, j) = i*nj + j

k2ij :: Int -> Int -> (Int, Int)
k2ij nj k = (k `div` nj, k`rem` nj)

isRunning' :: Game -> Bool
isRunning' g = g^.gameStatus == RedPlays || g^.gameStatus == BluePlays

play' :: Move -> Game -> Maybe Game
play' m g =
  if m `elem` g^.gameMoves
    then
      let
        -- apply move
        cell = g^.gameCurrentPlayer & player2cell
        board' = (g^.gameBoard) V.// [(ij2k (g^.gameNj) (m^.moveFrom), CellEmpty), (ij2k (g^.gameNj) (m^.moveTo), cell)]
        -- update status and current player
        winning = isWinning m g
        (status', player') =
          case (winning, g^.gameCurrentPlayer) of
            (True, PlayerRed)   -> (RedWins, PlayerRed)
            (True, PlayerBlue)  -> (BlueWins, PlayerBlue)
            (False, PlayerRed)  -> (BluePlays, PlayerBlue)
            (False, PlayerBlue) -> (RedPlays, PlayerRed)
        -- update moves
        moves' = if winning then [] else computeMoves board' (g^.gameNi) (g^.gameNj) player'
        -- if the next player has no move, the game is over
        (status'', player'') =
          case (moves', status') of
            ([], RedPlays)  -> (BlueWins, PlayerBlue)
            ([], BluePlays) -> (RedWins, PlayerRed)
            _               -> (status', player')
      in Just $ Game board' (g^.gameNi) (g^.gameNj) status'' moves' (g^.gameInitialPlayer) player''
    else Nothing

mkBoard :: Int -> Int -> Board
mkBoard ni nj =
    V.replicate (2*nj) CellBlue <>
    V.replicate ((ni-4)*nj) CellEmpty <>
    V.replicate (2*nj) CellRed

computeGame :: Int -> Int -> Player -> Game
computeGame ni nj p = Game board ni nj status moves p p
  where
    board = mkBoard ni nj
    moves = computeMoves board ni nj p
    status = if p == PlayerRed then RedPlays else BluePlays

computeMoves :: Board -> Int -> Int -> Player -> [Move]
computeMoves b ni nj = \case
  PlayerRed -> V.ifoldl' (f CellRed CellBlue (-1)) [] b
  PlayerBlue -> V.ifoldl' (f CellBlue CellRed 1) [] b
  where
    f cell1 cell2 deltaI acc k c =
      if c /= cell1
        then acc
        else
            [ Move (i0, j0) (i1, j1) 
            | let (i0, j0) = k2ij nj k
            , let i1 = i0 + deltaI
            , j1<-[j0-1 .. j0+1]
            , i1>=0 && i1<ni && j1>=0 && j1<nj      -- ij1 is inside the board
            , let c1 = b V.! ij2k nj (i1, j1)
            , c1==cell2 && j1/=j0 || c1==CellEmpty  -- move to an empty cell or capture (diagonaly) an opponent's cell
            ] ++ acc

player2cell :: Player -> Cell
player2cell = \case
  PlayerRed -> CellRed
  PlayerBlue -> CellBlue

isWinning :: Move -> Game -> Bool
isWinning m g =
  case g ^. gameCurrentPlayer of
    PlayerRed -> (m^.moveTo & fst) == 0
    PlayerBlue -> (m^.moveTo & fst) == (g^.gameNj - 1)

