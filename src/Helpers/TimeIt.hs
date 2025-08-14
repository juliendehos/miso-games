-- slightly modified version of:
-- https://github.com/merijn/timeit/blob/master/System/TimeIt.hs

module Helpers.TimeIt where

import Control.Monad.IO.Class
import System.CPUTime
import Text.Printf

myTimeIt :: Show a => String -> a -> IO (Double, a)
myTimeIt msg f = do
  putStrLn $ "\n*** " <> msg <> " ***"
  r@(t, _) <- timeItT $ let y = f in y `seq` putStrLn ("result: " <> show y) >> pure y
  printf "time: %6.6fs\n" t
  pure r

{-
myTimeIt :: MonadIO m => String -> a -> m ()
myTimeIt name f = timeItNamed name $ f `seq` pure ()
-}

-- | Wrap a 'MonadIO' computation so that it prints out the execution time.
timeIt :: MonadIO m => m a -> m a
timeIt = timeItNamed "CPU time"

-- | Like 'timeIt', but uses the 'show' rendering of @a@ as label for the
-- timing.
--
-- @since 2.0
timeItShow :: (MonadIO m, Show a) => m a -> m a
timeItShow ioa = do
    (t, a) <- timeItT ioa
    liftIO $ printf (show a ++ ": %6.6fs\n") t
    return a

-- | Like 'timeIt', but uses the 'String' as label for the timing.
--
-- @since 2.0
timeItNamed :: MonadIO m => String -> m a -> m a
timeItNamed name ioa = do
    (t, a) <- timeItT ioa
    liftIO $ printf (name ++ ": %6.6fs\n") t
    return a

-- | Wrap a 'MonadIO' computation so that it returns execution time in seconds,
-- as well as the result value.
timeItT :: MonadIO m => m a -> m (Double, a)
timeItT ioa = do
    t1 <- liftIO getCPUTime
    a <- ioa
    t2 <- liftIO getCPUTime
    let t :: Double
        t = fromIntegral (t2-t1) * 1e-12
    return (t, a)

