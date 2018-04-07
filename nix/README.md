# Nix expressions for GHC with -XDerivingVia

By default, this builds a GHC with a few packages
installed that we are using in the paper.

But it's easy to adapt the Nix expressions so that
a plain GHC is obtained.

## Enter a Nix shell with the patched ghc/ghci in path

```
nix-shell
```

## To merely build GHC with DerivingVia patch using Nix

```
nix-build
```
