{ opam-nix, opam-repository, hasSubmodules, ocamlVersion ? "5.2.1" }:
{ pkgs, system, }:
let

  lib = let on = opam-nix.lib.${system};
  in rec {
    patchesOverlay = final: prev: {
      # See https://github.com/tweag/opam-nix/issues/109
      conf-libpcre = prev.conf-libpcre.overrideAttrs (prev: {
        # We need to add the pkg-config path to the PATH so that dune can find
        # it TODO fix
        # https://github.com/ocaml/opam-repository/blob/master/packages/conf-libpcre/conf-libpcre.2/opam
        # to use pkg-conf on macos
        nativeBuildInputs = prev.nativeBuildInputs ++ [ pkgs.pkg-config ];
      });

      conf-libffi = prev.conf-libffi.overrideAttrs (prev: {
        # We need to add the pkg-config path to the PATH so that dune can find
        # it TODO fix
        # https://github.com/ocaml/opam-repository/blob/master/packages/conf-libffi/conf-libffi.2/opam
        # to use pkg-conf on macos
        nativeBuildInputs = prev.nativeBuildInputs ++ [ pkgs.pkg-config ];
      });
    };

    # helper to add buildinputs to an existing pkg
    addBuildInputs = pkg: inputs:
      pkg.overrideAttrs (prev: { buildInputs = prev.buildInputs ++ inputs; });

    # convert scopes to a list of pkgs so we can explicitly add packages from
    # the query
    scopeToPkgs = query: scope:
      builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames query) scope);

    # Pass a src and list of paths in that source to get a src that is only
    # those paths
    strictSrc = src: paths:
      # Use cleanSource, but limit it to only include srcs explicitly listed
      with pkgs.lib.fileset;
      (toSource {
        root = src;
        fileset = (intersection (fromSource (pkgs.lib.sources.cleanSource src))
          (unions paths));
      });

    # TODO https://github.com/tweag/opam-nix/blob/main/DOCUMENTATION.md#materialization
    # Will speed it up
    buildOpamPkg = { name, src, query ? { }
      , overlays ? [ patchesOverlay on.defaultOverlay ], inputs ? [ ] }:
      let
        # Force ocaml version
        baseQuery = {
          ocaml-base-compiler = ocamlVersion;
          # https://github.com/tweag/opam-nix/issues/112
          ocamlfind = "1.9.6";
        };
        repos = [ "${opam-repository}" ];
        # repos = opamRepos to force newest version of opam
        # pkgs = pkgs to force newest version of nixpkgs instead of using opam-nix's
        # overlays = to force the default and patches overlay
        scope = on.buildOpamProject { inherit pkgs repos overlays; } name src
          (baseQuery // query);
        inputsFromQuery = scopeToPkgs query scope;
      in addBuildInputs scope.${name} (inputs ++ inputsFromQuery);

    # make sure we have submodules
    # See https://github.com/NixOS/nix/pull/7862
    buildPhaseSubmoduleCheck = buildPhase:
      let

        buildPhaseFail = ''
          echo "Derivation won't build outside of a nix shell without submodules:"
          echo "  nix build '.?submodules=1#' # build from local sources"
          exit 1
        '';

      in if hasSubmodules then buildPhase else buildPhaseFail;
  };

  # Grab opam packages from opam file
  semgrepOpam = lib.buildOpamPkg {
    name = "semgrep";
    src = ./.;
    # You can force versions of certain packages here
    query = {
      # needed or else the newest version breaks. Not sure why this doesn't happen
      # outside nix
      mirage-runtime = "4.4.2";
      # need for OCTS
      tsort = "*";
    };
    # needed for octs and pcre2
    # TODO move to depexts
    inputs = (with pkgs; [ tree-sitter pcre2 ]);
  };

  devOptional = lib.buildOpamPkg {
    name = "optional";
    src = ./dev;
    query = { utop = "2.15.0"; };
  };

  devRequired = lib.buildOpamPkg {
    name = "required";
    src = ./dev;
  };
in let

  #
  # semgrep
  #

  darwinEnv = {
    # all the dune files of semgrep treesitter <LANG> are missing the
    # :standard field. Basically all compilers autodetct if something is c
    # or c++ based on file extension, and add the c stdlib based on that.
    # Nix doesn't because reasons:
    # https://github.com/NixOS/nixpkgs/issues/150655 Dune also passes
    # -xc++ if it detects a c++ file (again sane), but it's included in
    # the :standard var, which we don't add because ??? TODO add and
    # commit them instead of doing this
    NIX_CFLAGS_COMPILE = "-I${pkgs.libcxx.dev}/include/c++/v1";
  };
  env = {
    # Needed so we don't pass any flags in flags.sh
    SEMGREP_NIX_BUILD = "1";
  } // (if pkgs.stdenv.isDarwin then darwinEnv else { });
  semgrep = semgrepOpam.overrideAttrs (prev: rec {
    # Special environment variables for osemgrep for linking stuff

    src = (lib.strictSrc ./. (with pkgs.lib.fileset; [
      ./Makefile
      ./TCB
      ./bin
      # might be missing due to submodule issue (dumb)
      (maybeMissing ./cli/src/semgrep/semgrep_interfaces)
      ./dune
      ./dune-project
      ./interfaces
      ./languages
      ./libs
      ./src
      ./tools

      # only needed for testing
      # TODO split out into separate derivation
      ./cli/tests/default/e2e/targets/ls
      ./scripts/run-core-test
      ./scripts/make-symlinks
      ./test
      ./tests
    ]));

    inherit env;

    buildPhase = lib.buildPhaseSubmoduleCheck "make core";
    # needed for networking tests
    nativeCheckInputs = (with pkgs; [ cacert git ]);

    # git init is needed so tests work successfully since many rely on git root existing
    checkPhase = ''
      git init
      make test
    '';

    # DONE! Copy semgrep binaries!!!!
    installPhase = ''
      mkdir -p $out/bin
      cp _build/install/default/bin/* $out/bin
    '';

  });

  # for development
  devPkgs = devOptional.buildInputs ++ devRequired.buildInputs;
in {
  pkg = semgrep;
  devEnv = env;
  inherit devPkgs;
  inherit lib;
}
