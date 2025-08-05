# :ramen: miso-games

## Solo games

- [ ] [Minesweeper](https://en.wikipedia.org/wiki/Minesweeper)
- [ ] [Samegame](https://en.wikipedia.org/wiki/SameGame)

## Two-player games

- [ ] [Tic-tac-toe](https://en.wikipedia.org/wiki/Tic-tac-toe)
- [ ] [Connect Four](https://en.wikipedia.org/wiki/Connect_Four)
- [ ] [Reversi/Othello](https://en.wikipedia.org/wiki/Reversi)
- [ ] [Gomoku](https://en.wikipedia.org/wiki/Gomoku)

## Bots

- [ ] Random
- [ ] Monte-Carlo
- [ ] Monte-Carlo Tree Search

## Build and run

Install [Nix Flakes](https://nixos.wiki/wiki/Flakes), then:

```
nix develop .#wasm
make
make serve
```

