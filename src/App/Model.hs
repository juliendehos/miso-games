{-# LANGUAGE OverloadedStrings #-}

module App.Model where

import Control.Monad.ST
import Control.Monad.State.Lazy
import Miso.Lens
import Miso.Lens.TH
import System.Random

import Breakthrough.Model as Breakthrough
import ConnectFour.Model as ConnectFour
import Minesweeper.Model as Minesweeper
import Othello.Model as Othello
import Tictactoe.Model as Tictactoe

data GameType
  = Breakthrough
  | Breakthrough86
  | ConnectFour
  | Minesweeper
  | Othello
  | Tictactoe
  deriving (Eq)

data AppModel = AppModel
  { _modelGameType        :: GameType
  , _modelBreakthrough    :: Breakthrough.Model
  , _modelBreakthrough86  :: Breakthrough.Model
  , _modelConnectFour     :: ConnectFour.Model
  , _modelMinesweeper     :: Minesweeper.Model
  , _modelOthello         :: Othello.Model
  , _modelTictactoe       :: Tictactoe.Model
  } deriving (Eq)

makeLenses ''AppModel

#if MIN_VERSION_random(1,3,0)
mysplit :: SplitGen g => g -> (g, g)
mysplit = splitGen
#else
mysplit :: RandomGen g => g -> (g, g)
mysplit = split
#endif

getGen :: MonadState StdGen m => m StdGen
getGen = do
  g0 <- get
  let (g1, g1') = mysplit g0
  put g1
  pure g1'

mkAppModel :: StdGen -> AppModel
mkAppModel gen0 = runST $ flip evalStateT gen0 $ do
  AppModel Tictactoe
    <$> (Breakthrough.mkModel 8 8 <$> getGen)
    <*> (Breakthrough.mkModel 8 6 <$> getGen)
    <*> (ConnectFour.mkModel <$> getGen)
    <*> (Minesweeper.mkModel Minesweeper.ModeBeginner =<< getGen)
    <*> (Othello.mkModel <$> getGen)
    <*> (Tictactoe.mkModel <$> getGen)

