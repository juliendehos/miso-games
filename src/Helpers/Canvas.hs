
module Helpers.Canvas where

import Control.Monad (forM_)
import Miso.Canvas as Canvas
import Miso.Style as Style

drawBackground :: Color -> Double -> Double -> Canvas ()
drawBackground c w h = do
  fillStyle (Canvas.color c)
  fillRect (0, 0, w, h)

drawGrid :: Color -> Int -> Int -> Int -> Int -> Double -> Double -> Canvas ()
drawGrid c ni nj di dj w h = do
  fillStyle (Canvas.color c)
  beginPath ()
  forM_ [1 .. nj-1] $ \j -> do
    let x = fromIntegral (j * dj)
    moveTo (x, 0)
    lineTo (x, h)
  forM_ [1 .. ni-1] $ \i -> do
    let y = fromIntegral (i * di)
    moveTo (0, y)
    lineTo (w, y)
  Canvas.stroke ()

xy2ij' :: Int -> Int -> Double -> Double -> (Int, Int)
xy2ij' di dj x y = (floor y `div` di, floor x `div` dj)

-- top-left
ij2xyTL' :: Int -> Int -> Int -> Int -> (Double, Double)
ij2xyTL' di dj i j = (fromIntegral (j*dj), fromIntegral (i*di))

-- Center
ij2xyC' :: Int -> Int -> Int -> Int -> (Double, Double)
ij2xyC' di dj i j = 
  let x = (fromIntegral j + 0.5) * fromIntegral dj
      y = (fromIntegral i + 0.5) * fromIntegral di
  in (x, y)

