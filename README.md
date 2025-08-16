# :ramen: miso-games

## Solo games

- [x] [Minesweeper](https://en.wikipedia.org/wiki/Minesweeper)
- [ ] [Samegame](https://en.wikipedia.org/wiki/SameGame)

## Two-player games

- [x] [Tic-tac-toe](https://en.wikipedia.org/wiki/Tic-tac-toe)
- [x] [Breakthrough](https://en.wikipedia.org/wiki/Breakthrough_(board_game))
- [ ] [Gomoku](https://en.wikipedia.org/wiki/Gomoku)
- [ ] [Reversi/Othello](https://en.wikipedia.org/wiki/Reversi)

## Bots

- [x] Random
- [x] Monte-Carlo

## Build and run

Install [Nix Flakes](https://nixos.wiki/wiki/Flakes), then:

```sh
nix develop .#wasm

# build and run app
make build && make serve

# test
make test

# benchmark
make time

# build, test, optimize
make
```

test/bench using ghc:

```sh
nix develop .#default
cabal test
cabal run time
cabal bench
cabal build criterion && $(cabal list-bin criterion | tail -n 1) --output bench.html
$(cabal list-bin criterion | tail -n 1) --list
$(cabal list-bin criterion | tail -n 1) "Breakthrough.Game/play"
```

