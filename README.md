# :ramen: miso-games

## Solo games

- [x] [Minesweeper](https://en.wikipedia.org/wiki/Minesweeper)
- [ ] [Samegame](https://en.wikipedia.org/wiki/SameGame)

## Two-player games

- [x] [Tic-tac-toe](https://en.wikipedia.org/wiki/Tic-tac-toe)
- [ ] [Breakthrough](https://en.wikipedia.org/wiki/Breakthrough_(board_game))
- [ ] [Gomoku](https://en.wikipedia.org/wiki/Gomoku)
- [ ] [Reversi/Othello](https://en.wikipedia.org/wiki/Reversi)

## Bots

- [ ] Random
- [ ] Monte-Carlo

## Build and run

Install [Nix Flakes](https://nixos.wiki/wiki/Flakes), then:

```
nix develop .#wasm
make
make serve
```

