
import Breakthrough.GameBench
import Minesweeper.GameBench
import Tictactoe.GameBench

main :: IO ()
main = do
  Breakthrough.GameBench.run
  Minesweeper.GameBench.run
  Tictactoe.GameBench.run

