
module Game where

data Status
  = Player1Plays
  | Player2Plays
  | Player1Wins
  | Player2Wins
  | Draw
  deriving (Eq)

data Player
  = Player1
  | Player2
  deriving (Eq)

