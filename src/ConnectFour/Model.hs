{-# LANGUAGE OverloadedStrings #-}

module ConnectFour.Model where

import Control.Monad.State.Lazy
import Miso
import Miso.Lens
import Miso.Lens.TH
import System.Random

import Bot.MonteCarlo
import Bot.Random
import ConnectFour.Game

data PlayerType
  = Human
  | BotRandom
  | BotMcEasy
  | BotMcHard
  deriving (Eq)

data Model = Model
  { _modelGame            :: Game
  , _modelLog             :: MisoString
  , _modelPlayerYellow    :: PlayerType
  , _modelPlayerYellowGen :: StdGen
  } deriving (Eq)

makeLenses ''Model

mkModel :: StdGen -> Model
mkModel = Model mkGame "this is ConnectFour" Human

genMovePlayerYellow :: MonadState Model m => m (Maybe Move)
genMovePlayerYellow = do
  playerType <- use modelPlayerYellow
  game <- use modelGame
  gen <- use modelPlayerYellowGen
  let (move, gen') = case playerType of
          Human -> (Nothing, gen)
          BotRandom -> Bot.Random.genMove' game gen
          BotMcEasy -> Bot.MonteCarlo.genMove' 10 game gen
          BotMcHard -> Bot.MonteCarlo.genMove' 500 game gen
  modelPlayerYellowGen .= gen'
  pure move

