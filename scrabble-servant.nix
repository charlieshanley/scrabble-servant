{ mkDerivation, aeson, base, containers, lucid, mtl, servant-lucid
, servant-server, stdenv, text, warp
}:
mkDerivation {
  pname = "scrabble-servant";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers lucid mtl servant-lucid servant-server text
    warp
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/charlieshanley/scrabble-servant";
  description = "Web API for the words that can be made with a set of letters";
  license = stdenv.lib.licenses.bsd3;
}
