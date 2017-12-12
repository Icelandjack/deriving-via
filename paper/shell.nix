with import <nixpkgs> {};

let
  ghc-deriving-via = callPackage ../nix/ghc-deriving-via.nix {};
in
  stdenv.mkDerivation rec {
    name = "ghc-deriving-via-env";
    env = buildEnv {
      inherit name;
      paths = buildInputs;
    };
    buildInputs = [
      ghc-deriving-via
      haskellPackages.ghcid
      haskellPackages.lhs2tex
      (texlive.combine {
        inherit (texlive)
          scheme-basic
          collection-latexextra
          collection-latexrecommended
          collection-xetex
          collection-fontsrecommended
          kastrup
          latexmk
          stmaryrd
          ec
          libertine
          inconsolata
          newtx
          boondox
          mweights
          zapfding;
      })
    ];
  }
