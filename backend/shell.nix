{ nixpkgs ? import ../nixpkgs {} }:

let # Copied from <nixpkgs/pkgs/development/haskell-modules/lib.nix> so that -I isn't necessary
    overrideCabal = drv: f: (drv.override (args: args // {
      mkDerivation = drv: args.mkDerivation (drv // f drv);
    })) // {
      overrideScope = scope: overrideCabal (drv.overrideScope scope) f;
    };

in ((nixpkgs.haskell-ng.packages.ghc7101.override {
  overrides = self: super: {
    authenticate-oauth = overrideCabal super.authenticate-oauth (drv: {
      version = "1.5.1";
      src = ../authenticate/authenticate-oauth;
      sha256 = "1swqrqz3idc6zghwsx3yd1rpphgi74r5yp31rkvcik6dwzrgdn1f";
    });
    twitter-conduit = overrideCabal super.twitter-conduit (drv: {
      doCheck = false;
    });
  };
}).callPackage ./. {}).env
