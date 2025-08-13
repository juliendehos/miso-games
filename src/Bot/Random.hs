
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

genMove' :: (RandomGen gen, GameClass game move player) => game -> gen -> (Maybe move, gen)
genMove' game gen =
  case moves of
    [] -> (Nothing, gen)
    _ -> (Just (moves !! k), gen')
  where
    moves = getPossibleMoves game
    (k, gen') = uniformR (0, length moves - 1) gen
  
