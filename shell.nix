{ }:
let this = import ./.;
    reflexEnv = platform: (builtins.getAttr platform this).ghcWithPackages (p: with p; [
      reflex
      reflex-dom
      hscolour
      haskell-src-meta
      raw-strings-qq
      twitter-types
      twitter-types-lens
      lens
      lens-aeson
#      authenticate-oauth
    ]);
in this.nixpkgs.runCommand "shell" {
  buildCommand = ''
    echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs" > $out
  '';
  buildInputs = [
    this.nixpkgs.nodejs
  ] ++ builtins.map reflexEnv this.platforms;
} ""
