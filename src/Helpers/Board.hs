{-# LANGUAGE RecordWildCards #-}

module Helpers.Board where

import Data.Vector as V

data Board' a = Board'
  { _boardNi :: Int
  , _boardNj :: Int
  , _boardData :: Vector a
  } deriving (Eq)

mkBoardFromList :: Int -> Int -> [a] -> Board' a
mkBoardFromList ni nj = Board' ni nj . fromList

mkBoardFromVal :: Int -> Int -> a -> Board' a
mkBoardFromVal ni nj v = Board' ni nj (V.replicate (ni*nj) v)

getK :: Board' a -> Int -> a
getK Board'{..} k = _boardData ! k

getIJ :: Board' a -> Int -> Int -> a
getIJ b@Board'{..} i j = _boardData ! ij2k b i j

forBoard :: (Monad m) => Board' a -> (Int -> Int -> a -> m ()) -> m ()
forBoard b@Board'{..} f = V.iforM_ _boardData $ \k c -> 
    let (i, j) = k2ij b k
    in f i j c

ij2k :: Board' a -> Int -> Int -> Int
ij2k Board'{..} i j = i*_boardNj + j

k2ij :: Board' a -> Int -> (Int, Int)
k2ij Board'{..} k = (k `div` _boardNj, k`rem` _boardNj)

