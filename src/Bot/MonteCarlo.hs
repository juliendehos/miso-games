
module Bot.MonteCarlo where

import Control.Monad
import Control.Monad.State.Lazy
import System.Random

import Bot.Random
import Game

genMove' :: (RandomGen gen, GameClass game move player) => Int -> game -> gen -> (Maybe move, gen)
genMove' nSims game0 gen0 = (move1, gen1)
  where 
    player = getCurrentPlayer game0

    f acc@(bestMove, bestScore, gen) move =
      -- play the tested move
      case play move game0 of
        Nothing -> acc
        Just game' -> 
          -- evaluate the resulting game 
          let (score, gen') = computeScore' nSims player game' gen
          in if score > bestScore
            then (Just move, score, gen')
            else (bestMove, bestScore, gen')

    (move1, _, gen1) = foldl' f (Nothing, minBound::Int, gen0) (getPossibleMoves game0)


-- compute playouts and sum the scores
computeScore':: (RandomGen gen, GameClass game move player) => Int -> player -> game -> gen -> (Int, gen)
computeScore' nSims player game0 gen0 = go nSims gen0 0
  where
    go 0 gen score = (score, gen)
    go n gen score = 
      let (s, gen') = playout' player game0 gen
      in go (n-1) gen' (score+s)

-- randomly plays a game until the end, then computes its score
playout' :: (RandomGen gen, GameClass game move player) => player -> game -> gen -> (Int, gen)
playout' p game0 gen0 = (scoreForPlayer p game1, gen1)
  where
    go game gen =
      if isRunning game
        then 
          case Bot.Random.genMove' game gen of
            (Nothing, gen') -> (game, gen')
            (Just move, gen') -> 
              case play move game of
                Nothing -> (game, gen')
                Just game' -> go game' gen'
        else (game, gen)

    (game1, gen1) = go game0 gen0



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

{-
computeScore:: (MonadState StdGen m, GameClass game move player) => Int -> player -> game -> m Int
computeScore nSims player game = go nSims 0
  where
    go 0 score = pure score
    go n score = do
      s <- playout player game
      go (n - 1) (score + s)
-}

-- randomly plays a game until the end, then computes its score
playout :: (MonadState StdGen m, GameClass game move player) => player -> game -> m Int
playout p game0 = scoreForPlayer p <$> go game0 
  where
    go game =
      if isRunning game
        then do
          mGame <- (>>= (`play` game)) <$> Bot.Random.genMove game
          maybe (pure game) go mGame
        else pure game

