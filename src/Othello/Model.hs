{-# LANGUAGE OverloadedStrings #-}

module Othello.Model where

import Control.Monad.State.Lazy
import Miso
import Miso.Lens
import Miso.Lens.TH
import System.Random

import Bot.MonteCarlo
import Bot.Random
import Othello.Game

data PlayerType
  = Human
  | BotRandom
  | BotMcEasy
  | BotMcHard
  deriving (Eq)

data Model = Model
  { _modelGame            :: Game
  , _modelLog             :: MisoString
  , _modelPlayerWhite     :: PlayerType
  , _modelPlayerWhiteGen  :: StdGen
  } deriving (Eq)

makeLenses ''Model

mkModel :: StdGen -> Model
mkModel = Model mkGame "this is Othello" Human

genMovePlayerWhite :: MonadState Model m => m (Maybe Move)
genMovePlayerWhite = do
  playerType <- use modelPlayerWhite
  game <- use modelGame
  gen <- use modelPlayerWhiteGen
  let (move, gen') = case playerType of
          Human -> (Nothing, gen)
          BotRandom -> Bot.Random.genMove' game gen
          BotMcEasy -> Bot.MonteCarlo.genMove' 10 game gen    -- TODO
          BotMcHard -> Bot.MonteCarlo.genMove' 100 game gen   -- TODO
  modelPlayerWhiteGen .= gen'
  pure move

