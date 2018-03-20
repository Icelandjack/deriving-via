{ _nixpkgs ? import <nixpkgs> {} } :

let

  nixpkgs = import (_nixpkgs.fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "cc4677c36ee8d880e881459ad114fd2224b3ac1c";
    sha256 = "1rc1pjnvfi194gka45zc1nivzsncc819kvxlfv277l2c8ryhgbpc";
  }) { overlays = [ cabalHashes ]; };

  cabalHashes = self : super : {
    all-cabal-hashes = self.fetchurl {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/d561dd63393ec02602a95bb8c715111e9ddc3871.tar.gz";
      sha256 = "1qligvl2ivzfspcxgpw1hvb1r5v4lpv72pcijbsjsrhkwjzqbc9j";
    };
  };

in

  nixpkgs
