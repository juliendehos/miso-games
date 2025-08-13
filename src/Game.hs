{-# LANGUAGE FunctionalDependencies #-}

module Game where

class GameClass g m p | g -> m p  where
  -- TODO getPossibleMoves :: g -> Vector m ?
  getPossibleMoves :: g -> [m]
  getCurrentPlayer :: g -> p
  isRunning :: g -> Bool
  play :: m -> g -> Maybe g
  -- play :: m -> g -> g    -- TODO ?
  scoreForPlayer :: p -> g -> Int

