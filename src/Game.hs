{-# LANGUAGE FunctionalDependencies #-}

module Game where

-- TODO write a Game typeclass, for implementing Bot

class GameClass g m | g -> m where
  getPossibleMoves :: g -> [m]

