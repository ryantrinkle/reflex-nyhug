{ mkDerivation, bytestring, twitter-conduit, authenticate-oauth, snap-server, websockets, websockets-snap }:

mkDerivation {
  pname = "reflex-talk-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ bytestring twitter-conduit authenticate-oauth snap-server websockets-snap websockets ];
  license = null;
}
