# Scrabble Servant

A web API for the words that can be made with a set of letters.

### To provision development environment with nix

`nix-shell` will automatically evaluate shell.nix, which gets the environment
attribute of the haskell package returned by default.nix and opens a nix-shell
appropriately provisioned.

Within this environment, cabal can be used as normal (`cabal repl`, `cabal build`).

ghcid.sh is provided as a convenience to quickly open ghcid with cabal repl as
the command.

Note that scrabble-servant.nix is derived from scrabble-servant.cabal via
`cabal2nix . > scrabble-servant.nix`, and this must be done each time
scrabble-servant.cabal is modified.

### To build

The package can be built with either Nix or Cabal:

- `nix-build` will build with nix according to instructions from default.nix and
scrabble-servant.nix.
- Within nix-shell, `cabal build` can be used to build with cabal.

---

For more info about using Nix with Haskell, see Gabriel Gonzalez's 
[tutorial](https://github.com/Gabriel439/haskell-nix/).

