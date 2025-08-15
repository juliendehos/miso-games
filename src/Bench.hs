
import Criterion.Main

import Bot.MonteCarloBench
import Breakthrough.GameBench
import Minesweeper.GameBench
import Tictactoe.GameBench

main :: IO ()
main = 
  defaultMain 
    (  Bot.MonteCarloBench.groups
    <> Breakthrough.GameBench.groups
    <> Minesweeper.GameBench.groups
    <> Tictactoe.GameBench.groups
    )

