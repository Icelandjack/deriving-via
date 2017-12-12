with import <nixpkgs> {};

let
  ghc-deriving-via = callPackage ./ghc-deriving-via.nix {};
in
  stdenv.mkDerivation rec {
    name = "ghc-deriving-via-env";
    env = buildEnv {
      inherit name;
      paths = buildInputs;
    };
    buildInputs = [ ghc-deriving-via ];
  }
