# Scrabble Servant

A web API to suggest scrabble letters given your seven tiles.

### To provision development environment with nix

`nix-shell`

nix-shell will automatically evaluate shell.nix, which gets the environment
attribute from the haskell package returned by default.nix

For more info, see Gabriel Gonzalez's [tutorial](https://github.com/Gabriel439/haskell-nix/)

Note that `scrabble-servant.nix` is derived from `scrabble-servant.cabal` via
`cabal2nix`.
