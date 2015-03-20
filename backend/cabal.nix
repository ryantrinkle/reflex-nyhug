{ mkDerivation, bytestring, twitter-conduit, authenticate-oauth }:

mkDerivation {
  pname = "reflex-talk-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ bytestring twitter-conduit authenticate-oauth ];
  license = null;
}
