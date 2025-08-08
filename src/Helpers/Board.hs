{-# LANGUAGE RecordWildCards #-}

module Helpers.Board where

import Miso.Lens

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

getK :: Board' a -> Int -> a
getK Board'{..} k = _boardData ! k

getIJ :: Board' a -> Int -> Int -> a
getIJ b@Board'{..} i j = _boardData ! ij2k b i j

setK :: Board' a -> Int -> a -> Board' a
setK b k v = b & boardData %~ (// [(k, v)])

setIJ :: Board' a -> Int -> Int -> a -> Board' a
setIJ b i j = setK b (ij2k b i j)

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

