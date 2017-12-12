let
  nixpkgs = import <nixpkgs> {};
  ghc-deriving-via = (import ./ghc-deriving-via.nix).ghc-deriving-via;
in
  nixpkgs.stdenv.mkDerivation rec {
    name = "ghc-deriving-via-env";
    env = nixpkgs.buildEnv {
      inherit name;
      paths = buildInputs;
    };
    buildInputs = [ ghc-deriving-via ];
  }
