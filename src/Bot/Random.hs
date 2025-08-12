
module Bot.Random where

import Control.Monad.State.Lazy
import System.Random

import Game

genMove :: (MonadState StdGen m, GameClass game move player) => game -> m (Maybe move)
genMove game = do
  let moves = getPossibleMoves game
  case moves of
    [] -> pure Nothing
    _ -> do
      k <- state (uniformR (0, length moves - 1))
      pure $ Just (moves !! k)

{-
genMove :: GameClass game move => game -> StdGen -> (Maybe move, StdGen)
genMove game gen = 
  let moves = getPossibleMoves game
  in case moves of
        [] -> (Nothing, gen)
        _ ->  let (k, gen') = uniformR (0, length moves - 1) gen
              in (Just (moves !! k), gen')
-}

