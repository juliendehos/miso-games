
module Helpers.Board where

import Data.Vector

data Board a = Board 
  { _boardNi :: Int
  , _boardNj :: Int
  , _boardData :: Vector a
  } deriving (Eq)

-- TODO check ni*nj == length list
mkBoard :: Int -> Int -> [a] -> Board a
mkBoard ni nj = Board ni nj . fromList



