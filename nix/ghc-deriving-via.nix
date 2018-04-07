{ stdenv
, writeText
, cacert
, git
, haskell
} :
let

  # We need a version of fetchgit that allows us to register two
  # remotes for the main repo, so that the submodules which use
  # relative paths are all pointing to the correct location.
  fetchgit-ghc =
    { origin   # location of master and all relative submodules
    , url      # location of fork
    , commit   # commit hash we want to build
    , ref      # git ref / branch we want to fetch
    , sha256   # checksum of the complete checkout
    , name     # nix-store name of the sources
    } :
    stdenv.mkDerivation
      { inherit name;
        builder = writeText "builder.sh" ''
          source $stdenv/setup

          header "exporting $url (branch $ref, commit $commit) into $out"

          mkdir -p "$out"
          cd "$out"

          git init
          # We add the origin repo (to which all the submodules are relative).
          git remote add origin "$origin";
          # We add the fork repo (containing the patches we're interested in).
          git remote add fork "$url";
          ( [ -n "$http_proxy" ] && git config http.proxy "$http_proxy" ) || true

          # Obtain the main repo.
          git fetch --progress --depth 1 fork +"$ref" || return 1
          git checkout -b local "$commit"

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
        inherit url origin commit ref;
        GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";
        impureEnvVars = stdenv.lib.fetchers.proxyImpureEnvVars ++ [
          "GIT_PROXY_COMMAND" "SOCKS_SERVER"
        ];
      };

in

  # This builds the fork of GHC with the desired changes.
  #
  # We do so by overriding the ghcHEAD expression which does almost
  # the right thing already (namely building GHC via the git repos).
  #
  # Note that we specify an explicit commit. This should result
  # in a fully reproducable build, but it means changes are not
  # picked up automatically.
  #
  (haskell.compiler.ghcHEAD.override { version = "8.5.20180406"; })
    .overrideAttrs
      (old :
        { src = fetchgit-ghc {
            name   = "ghc-deriving-via.git"; # store name for the sources
            origin = "git://git.haskell.org/ghc.git"; # primary repo
            url    = "git://github.com/ryanglscott/ghc.git"; # our fork
            ref    = "refs/heads/deriving-via-8.5"; # branch we want
            commit = "ead2f6ac232225e987b3fbbe30d88b134a3ff954"; # commit we want
            sha256 = "10i5n4sjijzvnlbd2rpy3z17xqpxz0dm0l9z7zfkh8yf3ksq25qy";
          };
        }
      )

