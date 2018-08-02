with (import ./nixpkgs.nix) {};

let

  ghc-deriving-via = callPackage ./ghc-deriving-via.nix {};
  ghc-deriving-via-pkgs = with haskell.lib; haskell.packages.ghc861.override {
    # ghc = ghc-deriving-via; # no longer using our build because 8.6.1 is in nixpkgs
    overrides = self : super : {
      mkDerivation = args : super.mkDerivation (args // {
        doCheck = false;
        jailbreak = true;
      });
      free                 = self.callHackage "free"                 "5"       {};
      generic-deriving     = self.callHackage "generic-deriving"     "1.12.1"  {};
      lens                 = self.callHackage "lens"                 "4.16"    {};
      primitive            = self.callHackage "primitive"            "0.6.3.0" {};
      profunctors          = self.callHackage "profunctors"          "5.2.2"   {};
      semigroupoids        = self.callHackage "semigroupoids"        "5.2.2"   {};
      tagged               = overrideCabal
        (self.callHackage "tagged"               "0.8.5"   {})
        (x : { preConfigure = ''sed -i "s:template-haskell.*:template-haskell:" tagged.cabal''; });
      text                 = self.callHackage "text"                 "1.2.3.0" {};
      unordered-containers = self.callHackage "unordered-containers" "0.2.9.0" {};
    };
  };
  ghc-deriving-via-with-pkgs =
    ghc-deriving-via-pkgs.ghcWithPackages ( pkgs : [
      pkgs.aeson
      pkgs.generic-deriving
      pkgs.generics-sop
      pkgs.mtl
      pkgs.QuickCheck
      pkgs.semigroups
      pkgs.transformers-compat
    ]);

in

  ghc-deriving-via-with-pkgs
