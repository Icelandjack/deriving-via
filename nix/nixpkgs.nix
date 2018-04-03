{ _nixpkgs ? import <nixpkgs> {} } :

let

  nixpkgs = import (_nixpkgs.fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "56fb68dcef494b7cdb3e09362d67836b8137019c";
    sha256 = "0im8l87nghsp4z7swidgg2qpx9mxidnc0zs7a86qvw8jh7b6sbv2";
  }) { overlays = [ cabalHashes ]; };

  cabalHashes = self : super : {
    all-cabal-hashes = self.fetchurl {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/4723c06cd8731f4435bba1d45217557860a6fdf7.tar.gz";
      sha256 = "0n7rawb5zlgmmhgf0nxri0w1sp6rh4ip6fdawwm0yibn30byy0sg";
    };
  };

in

  nixpkgs
