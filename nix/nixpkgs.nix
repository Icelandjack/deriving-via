{ _nixpkgs ? import <nixpkgs> {} } :

let

  nixpkgs = import (_nixpkgs.fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "5acbe81573523cf3e64d37b03539d7083459ac42";
    sha256 = "0w0i88cdff89spzplhx546cdm5ijyka6q57f67569gk9xk84dcy4";
  }) { overlays = [ cabalHashes ]; };

  cabalHashes = self : super : {
    all-cabal-hashes = self.fetchurl {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/60dcda750b22b08f41b3430a1a257e04208668e2.tar.gz";
      sha256 = "0bbq373jhn37lvn976yh5c2h5na9br2vpxr3zg96kncg6va8949a";
    };
  };

in

  nixpkgs
