with (import ./nixpkgs.nix) {};

let

  ghc-deriving-via = callPackage ./ghc-deriving-via.nix {};
  ghc-deriving-via-pkgs = with haskell.lib; haskell.packages.ghcHEAD.override {
    ghc = ghc-deriving-via;
    overrides = self : super : {
      mkDerivation = args : super.mkDerivation (args // {
        doCheck = false;
        jailbreak = true;
      });
      free                 = self.callHackage "free"                 "5"       {}; # super.free_5;
      generic-deriving     = self.callHackage "generic-deriving"     "1.12.1"  {};
      lens                 = self.callHackage "lens"                 "4.16"    {};
      primitive            = self.callHackage "primitive"            "0.6.3.0" {};
      profunctors          = self.callHackage "profunctors"          "5.2.2"   {};
      semigroupoids        = self.callHackage "semigroupoids"        "5.2.2"   {};
      text                 = self.callHackage "text"                 "1.2.3.0" {};
      unordered-containers = self.callHackage "unordered-containers" "0.2.9.0" {};
      # unordered-containers =
      #   appendPatch (self.callHackage "unordered-containers" "0.2.8.0" {}) ./patches/unordered-containers-0.2.8.0.patch;
    };
  };
  ghc-deriving-via-with-pkgs =
    ghc-deriving-via-pkgs.ghcWithPackages ( pkgs : [
      pkgs.aeson
      pkgs.generic-deriving
      pkgs.generic-lens
      pkgs.generics-sop
      pkgs.lens
      pkgs.mtl
      pkgs.profunctors
      pkgs.QuickCheck
      pkgs.semigroups
    ]);

in

  ghc-deriving-via-with-pkgs
