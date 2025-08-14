{-# LANGUAGE OverloadedStrings #-}

module Breakthrough.Model where

import Control.Monad.State.Lazy
import Miso
import Miso.Lens
import Miso.Lens.TH
import System.Random

import Bot.MonteCarlo
import Bot.Random
import Breakthrough.Game

data PlayerType
  = Human
  | BotRandom
  | BotMcEasy
  | BotMcMedium
  deriving (Eq)

data Model = Model
  { _modelGame          :: Game
  , _modelSelected      :: Maybe (Int, Int)
  , _modelLog           :: MisoString
  , _modelPlayerBlue    :: PlayerType
  , _modelPlayerBlueGen :: StdGen
  } deriving (Eq)

makeLenses ''Model

mkModel :: Int -> Int -> StdGen -> Model
mkModel ni nj = Model (mkGame ni nj) Nothing msg Human
  where
    msg = ms $ "this is Breakthrough " <> show ni <> "x" <> show nj

genMovePlayerBlue :: MonadState Model m => m (Maybe Move)
genMovePlayerBlue = do
  playerType <- use modelPlayerBlue
  game <- use modelGame
  gen <- use modelPlayerBlueGen
  let (move, gen') = case playerType of
          Human -> (Nothing, gen)
          BotRandom -> Bot.Random.genMove' game gen
          BotMcEasy -> Bot.MonteCarlo.genMove' 30 game gen
          BotMcMedium -> Bot.MonteCarlo.genMove' 300 game gen
          -- BotRandom -> runState (Bot.Random.genMove game) gen
          -- BotMcEasy -> runState (Bot.MonteCarlo.genMove 20 game) gen
          -- BotMcMedium -> runState (Bot.MonteCarlo.genMove 200 game) gen
  modelPlayerBlueGen .= gen'
  pure move

