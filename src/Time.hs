
import Bot.MonteCarloTime
import Breakthrough.GameTime
import Minesweeper.GameTime
import Tictactoe.GameTime

main :: IO ()
main = do
  Bot.MonteCarloTime.run
  Breakthrough.GameTime.run
  Minesweeper.GameTime.run
  Tictactoe.GameTime.run

