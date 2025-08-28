#! /bin/sh

echo ""
echo ""###############################################################################
echo "# clean previous build"
echo ""###############################################################################
make clean

echo ""
echo ""###############################################################################
echo "# wasm: build/test/optim"
echo ""###############################################################################
nix develop .#wasm --experimental-features "nix-command flakes" --command bash -c "make"

echo ""
echo ""###############################################################################
echo "# wasm: time"
echo ""###############################################################################
nix develop .#wasm --experimental-features "nix-command flakes" --command bash -c "make time"

echo ""
echo ""###############################################################################
echo "# ghc: build app"
echo ""###############################################################################
nix develop .#default --experimental-features "nix-command flakes" --command bash -c "cabal build app"

echo ""
echo ""###############################################################################
echo "# ghc: test"
echo ""###############################################################################
nix develop .#default --experimental-features "nix-command flakes" --command bash -c "cabal test"

echo ""
echo ""###############################################################################
echo "# ghc: time"
echo ""###############################################################################
nix develop .#default --experimental-features "nix-command flakes" --command bash -c "cabal run time"

echo ""
echo ""###############################################################################
echo "# ghc: bench"
echo ""###############################################################################
nix develop .#default --experimental-features "nix-command flakes" --command bash -c "cabal bench"

