let # Copied from <nixpkgs/pkgs/development/haskell-modules/lib.nix> so that -I isn't necessary
    overrideCabal = drv: f: (drv.override (args: args // {
      mkDerivation = drv: args.mkDerivation (drv // f drv);
    })) // {
      overrideScope = scope: overrideCabal (drv.overrideScope scope) f;
    };

    nixpkgs = import ./nixpkgs {config.allowUnfree = true;};
    extendHaskellPackages = haskellPackages: haskellPackages.override {
      overrides = self: super: {
        reflex = self.callPackage ./reflex {};
        reflex-dom = self.callPackage ./reflex-dom {};
        ghcjs-dom = overrideCabal super.ghcjs-dom (drv: {
          src = nixpkgs.fetchgit {
            url = git://github.com/xionite/ghcjs-dom.git;
            rev = "2d5a0aa64454f2c084b1a7c53cadb59b274d1386";
            sha256 = "8450b1a0de67cf6bb6c304c244e211331da8f5befdf92c089498c4394c14fcc2";
          };
        });
        snap = overrideCabal super.snap (drv: {
          editedCabalFile = "13r9q0cmfkysvgg3xs64k4ar3wdbpp25hzccphclimfnwvivdzig";
        });
      };
    };
in rec {
  inherit nixpkgs;
  ghc = extendHaskellPackages nixpkgs.pkgs.haskellPackages;
  ghcjs = extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghcjs;
  platforms = [ "ghcjs" ] ++ (if !nixpkgs.stdenv.isDarwin then [ "ghc" ] else []);
}
