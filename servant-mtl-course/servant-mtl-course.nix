{ mkDerivation, aeson, base, containers, directory, lens, mtl
, servant, servant-server, stdenv, stm, text, transformers, warp
}:
mkDerivation {
  pname = "servant-mtl-course";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers directory lens mtl servant servant-server stm
    text transformers warp
  ];
  testHaskellDepends = [ base ];
  description = "Build skills with MTL and Servant";
  license = stdenv.lib.licenses.bsd3;
}
