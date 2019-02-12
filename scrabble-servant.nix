{ mkDerivation, base, servant-lucid, servant-server, stdenv }:
mkDerivation {
  pname = "scrabble-servant";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base servant-lucid servant-server ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/charlieshanley/scrabble-servant";
  description = "API to suggest scrabble words that use your letters";
  license = stdenv.lib.licenses.bsd3;
}
