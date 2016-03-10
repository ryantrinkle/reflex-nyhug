{}:

let this = import ./common.nix;

in this.nixpkgs.stdenv.mkDerivation rec {
  name = "reflex-talk";
  src = ./frontend;
  backend = import ./backend {};
  builder = builtins.toFile "builder.sh" ''
    source "$stdenv/setup"

    cp -r "$src" "$out"
    chmod -R u+w "$out"
    cd "$out"

    mkdir -p bin
    ln -s "$backend/bin/reflex-talk-backend" bin/backend

    ghcjs --make -O2 -isrc src/Main.hs -o static/result
    closure-compiler --compilation_level=ADVANCED_OPTIMIZATIONS --js static/result.jsexe/all.js > static/result.jsexe/all.min.js
  '';
  buildInputs = [
    (this.ghc.ghcWithPackages (p: with p; [ snap ]))
    (this.ghcjs.ghcWithPackages (p: with p; [
      reflex
      reflex-dom
      hscolour
      haskell-src-meta
      raw-strings-qq
      twitter-types
      twitter-types-lens
      lens
      lens-aeson
      http-types
      network-uri
    ]))
    this.nixpkgs.nodejs
    this.nixpkgs.closurecompiler
  ];
}
