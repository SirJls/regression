{ mkDerivation, base, bed-and-breakfast, bytestring, Cabal, cassava
, decimal-arithmetic, filepath, genetic-algorithm
, optparse-applicative, statistics, stdenv, vector
}:
mkDerivation {
  pname = "regression";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bed-and-breakfast bytestring Cabal cassava decimal-arithmetic
    filepath genetic-algorithm optparse-applicative statistics vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
