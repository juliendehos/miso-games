{-# LANGUAGE FunctionalDependencies #-}

module Game where

class GameClass g m p | g -> m p  where
  getPossibleMoves :: g -> [m]
  getCurrentPlayer :: g -> p
  isRunning :: g -> Bool
  play :: m -> g -> Maybe g
  scoreForPlayer :: p -> g -> Int

