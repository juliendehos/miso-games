
import Bot.MonteCarloBench
import Breakthrough.GameBench
import Minesweeper.GameBench
import Tictactoe.GameBench

main :: IO ()
main = do
  Bot.MonteCarloBench.run
  Breakthrough.GameBench.run
  Minesweeper.GameBench.run
  Tictactoe.GameBench.run

