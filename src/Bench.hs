
import Criterion.Main

import Breakthrough.GameBench
import Bot.MonteCarloBench
import Minesweeper.GameBench
import Tictactoe.GameBench

main :: IO ()
main =
  defaultMain . concat =<< sequence 
    [ Breakthrough.GameBench.mkGroups
    , Bot.MonteCarloBench.mkGroups
    , Minesweeper.GameBench.mkGroups 
    , Tictactoe.GameBench.mkGroups
    ]

