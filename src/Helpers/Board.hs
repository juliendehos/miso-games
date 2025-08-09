{-# LANGUAGE RecordWildCards #-}

module Helpers.Board where

import Miso.Lens

import Prelude as P
import Data.Vector as V

-------------------------------------------------------------------------------
-- type
-------------------------------------------------------------------------------

data Board' a = Board'
  { _boardNi :: Int
  , _boardNj :: Int
  , _boardData :: Vector a
  } deriving (Eq)

-------------------------------------------------------------------------------
-- lenses
-------------------------------------------------------------------------------

boardNi :: Lens (Board' a) Int
boardNi = lens _boardNi (\ record field -> record {_boardNi = field})

boardNj :: Lens (Board' a) Int
boardNj = lens _boardNj (\ record field -> record {_boardNj = field})

boardData :: Lens (Board' a) (Vector a)
boardData = lens _boardData (\ record field -> record {_boardData = field})

-------------------------------------------------------------------------------
-- functions
-------------------------------------------------------------------------------

mkBoardFromList :: Int -> Int -> [a] -> Board' a
mkBoardFromList ni nj = Board' ni nj . fromList

mkBoardFromVal :: Int -> Int -> a -> Board' a
mkBoardFromVal ni nj v = Board' ni nj (V.replicate (ni*nj) v)

getK :: Int -> Board' a -> a
getK k b = (b^.boardData) ! k

getIJ :: Int -> Int -> Board' a -> a
getIJ i j b = (b^.boardData) ! ij2k b i j

setK :: Int -> a -> Board' a -> Board' a
setK k v b = b & boardData %~ (// [(k, v)])

setIJ :: Int -> Int -> a -> Board' a -> Board' a
setIJ i j v b = setK (ij2k b i j) v b

setIJs :: [((Int, Int), a)] -> Board' a -> Board' a
setIJs ijvs b = b & boardData %~ (// kvs)
  where
    kvs = P.map (\((i, j), v) -> (ij2k b i j, v)) ijvs

forBoard :: (Monad m) => Board' a -> (Int -> Int -> a -> m ()) -> m ()
forBoard b@Board'{..} f = V.iforM_ _boardData $ \k c -> 
    let (i, j) = k2ij b k
    in f i j c

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

ij2k :: Board' a -> Int -> Int -> Int
ij2k Board'{..} i j = i*_boardNj + j

k2ij :: Board' a -> Int -> (Int, Int)
k2ij Board'{..} k = (k `div` _boardNj, k`rem` _boardNj)

