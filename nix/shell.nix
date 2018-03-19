with (import ./nixpkgs.nix) {};

let
  ghc-deriving-via-with-pkgs = import ./default.nix;
in
  stdenv.mkDerivation rec {
    name = "ghc-deriving-via-env";
    env = buildEnv {
      inherit name;
      paths = buildInputs;
    };
    buildInputs = [ ghc-deriving-via-with-pkgs ];
  }
