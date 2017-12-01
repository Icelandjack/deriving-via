with (import <nixpkgs> {}).pkgs;

rec {

  # We need a version of fetchgit that allows us to register two
  # remotes for the main repo, so that the submodules which use
  # relative paths are all pointing to the correct location.
  fetchgit-ghc =
    { origin, url, rev, sha256, name } :
    stdenv.mkDerivation
      { inherit name;
        builder = writeText "builder.sh" ''
          source $stdenv/setup

          # Return the hash of a reference if it exists in the remote repo.
          hash_from_ref() {
            local ref=$1
            git ls-remote fork | sed -n "\,\t$ref, { s,\(.*\)\t\(.*\),\1,; p; q}"
          }

          header "exporting $url (rev $rev) into $out"

          mkdir -p "$out"
          cd "$out"

          git init
          # We add the origin repo (to which all the submodules are relative).
          git remote add origin "$origin";
          # We add the fork repo (containing the patches we're interested in).
          git remote add fork "$url";
          ( [ -n "$http_proxy" ] && git config http.proxy "$http_proxy" ) || true

          # Obtain the main repo.
          hash=$(hash_from_ref "$rev")
          git fetch --progress --depth 1 fork +"$rev" || return 1
          git checkout -b local "$hash"

          # Get all the submodules.
          git submodule init
          git submodule update # fetches all but seems difficult to avoid

          # For all submodules, delete the .git directories.
          echo "removing \`.git'..." >&2
          find "$out" -name .git -print0 | xargs -0 rm -rf

        '';
        buildInputs = [git];
        outputHashAlgo = "sha256";
        outputHashMode = "recursive";
        outputHash = sha256;
        inherit url origin rev;
        GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";
        impureEnvVars = stdenv.lib.fetchers.proxyImpureEnvVars ++ [
          "GIT_PROXY_COMMAND" "SOCKS_SERVER"
        ];
      };

  # This builds the fork of GHC with the desired changes.
  #
  # We do so by overriding the ghcHEAD expression which does almost
  # the right thing already (namely building GHC via the git repos).
  ghc-deriving-via-override =
    (haskell.compiler.ghcHEAD.override { version = "8.3.20171129"; })
      .overrideAttrs
        (old :
          { src = fetchgit-ghc {
              name   = "ghc-deriving-via.git"; # store name for the sources
              origin = "git://git.haskell.org/ghc.git"; # primary repo
              url    = "git://github.com/ryanglscott/ghc.git"; # our fork
              rev    = "refs/heads/deriving-via"; # branch / commit we want
              sha256 = "13qmvnn7nk15ndfy5xb00fdlf3ncjkj4djg9scqwp325i7g841zk";
            };

            # Set build flavour to devel2. For some reason, the default build
            # flavour fails on haddock, so at least we have to disable that.
            preConfigure = old.preConfigure + ''
              sed 's|#BuildFlavour.*=.*quickest|BuildFlavour = devel2|' mk/build.mk.sample > mk/build.mk
            '';

            # Mostly copied from head.nix, but removed the paxmarking of haddock,
            # because it does not exist in our build.
            postInstall =
              with lib.strings;
              let
                newPaxmark = ''
                  paxmark m $out/lib/${old.name}/bin/ghc
                '';
                modifiedOld =
                  concatStringsSep "\n" (lib.drop 1 (splitString "\n" old.postInstall));
              in
                newPaxmark + modifiedOld;
          }
        );
}

