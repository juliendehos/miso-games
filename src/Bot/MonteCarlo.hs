
module Bot.MonteCarlo where

import Control.Monad
import Control.Monad.State.Lazy
import System.Random

import Bot.Random
import Game

genMove :: (MonadState StdGen m, GameClass game move player) => Int -> game -> m (Maybe move)
genMove nSims game = 
  case getPossibleMoves game of
    [] -> pure Nothing
    ms@(m:_) -> Just . fst <$> foldM f (m, minBound::Int) ms

  where
    player = getCurrentPlayer game

    f acc@(_bestMove, bestScore) m =
      -- play the tested move
      case play m game of
        Nothing -> pure acc
        Just game1 -> do
          -- evaluate the resulting game 
          score <- computeScore nSims player game1
          if score > bestScore
            then pure (m, score)
            else pure acc

-- compute playouts and sum the scores
computeScore:: (MonadState StdGen m, GameClass game move player) => Int -> player -> game -> m Int
computeScore nSims player game = sum <$> replicateM nSims (playout player game)

-- randomly plays a game until the end, then computes its score
playout :: (MonadState StdGen m, GameClass game move player) => player -> game -> m Int
playout p game0 = scoreForPlayer p <$> go game0 
  where
    go game =
      if isRunning game
        then do
          mGame <- (>>= (`play` game)) <$> Bot.Random.genMove game
          -- maybe (pure game) go mGame   -- TODO test perf
          case mGame of
            Nothing -> pure game
            Just game' -> go game'
        else pure game

