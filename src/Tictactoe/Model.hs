{-# LANGUAGE OverloadedStrings #-}

module Tictactoe.Model where

import Control.Monad.State.Lazy
import Miso
import Miso.Lens
import Miso.Lens.TH
import System.Random

import Bot.Random
import Tictactoe.Game

data PlayerType
  = Human
  | BotRandom
  -- | BotMcEasy   -- TODO
  -- | BotMcHard
  deriving (Eq)

data Model = Model
  { _modelGame        :: Game
  , _modelLog         :: MisoString
  , _modelPlayerO     :: PlayerType
  , _modelPlayerOGen  :: StdGen
  } deriving (Eq)

makeLenses ''Model

mkModel :: StdGen -> Model
mkModel = Model mkGame "this is Tictactoe" Human

genMovePlayerO :: MonadState Model m => m (Maybe Move)
genMovePlayerO = do
  playerType <- use modelPlayerO
  game <- use modelGame
  gen <- use modelPlayerOGen
  let (move, gen') = case playerType of
          Human -> (Nothing, gen)
          BotRandom -> runState (Bot.Random.genMove game) gen
          -- BotMcEasy -> TODO
          -- BotMcHard -> TODO
  modelPlayerOGen .= gen'
  pure move

