{
  opam-nix,
  opam-repository,
  hasSubmodules,
  ocamlVersion ? "5.3.0",
}:
{ pkgs, system }:
let

  lib =
    let
      on = opam-nix.lib.${system};
    in
    rec {
      patchesOverlay = final: prev: {
        # If packages need added build inputs to build properly, add them here
        # Make sure to create an issue on opam-nix to upstream the fix!
      };

      # helper to add buildinputs to an existing pkg
      addBuildInputs =
        pkg: inputs:
        pkg.overrideAttrs (prev: {
          buildInputs = prev.buildInputs ++ inputs;
        });

      # convert scopes to a list of pkgs so we can explicitly add packages from
      # the query
      scopeToPkgs =
        query: scope: builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames query) scope);

      # Pass a src and list of paths in that source to get a src that is only
      # those paths
      strictSrc =
        src: paths:
        # Use cleanSource, but limit it to only include srcs explicitly listed
        with pkgs.lib.fileset;
        (toSource {
          root = src;
          fileset = (intersection (fromSource (pkgs.lib.sources.cleanSource src)) (unions paths));
        });

      # set doNixSupport to false so we don't accidentally drag in any conflicting deps
      # (since we only care about binaries for these)
      disableNixSupport =
        pkg:
        pkg.overrideAttrs (prev: {
          doNixSupport = false;
        });
      # TODO https://github.com/tweag/opam-nix/blob/main/DOCUMENTATION.md#materialization
      # Will speed it up
      buildOpamPkg =
        {
          name,
          src,
          query ? { },
          overlays ? [
            patchesOverlay
            on.defaultOverlay
          ],
          inputs ? [ ],
        }:
        let
          # Force ocaml version
          #
          # you can also force specific ocaml package versions like
          #
          # ocamlfind = "1.9.8";
          baseQuery = {
            # Not used currently as we pin ocaml-variants in semgrep.opam.template
            # ocaml-base-compiler = ocamlVersion;
            ocaml-option-flambda = "*";
          };
          resolveArgs = {
            # speeds up so we don't get a solver timeout
            criteria = null;
          };
          repos = [ "${opam-repository}" ];
          # repos = opamRepos to force newest version of opam
          # pkgs = pkgs to force newest version of nixpkgs instead of using opam-nix's
          # overlays = to force the default and patches overlay
          scope = on.buildOpamProject {
            inherit
              pkgs
              repos
              overlays
              resolveArgs
              ;
          } name src (baseQuery // query);
          inputsFromQuery = scopeToPkgs query scope;
        in
        addBuildInputs scope.${name} (inputs ++ inputsFromQuery);

      # make sure we have submodules
      # See https://github.com/NixOS/nix/pull/7862
      buildPhaseSubmoduleCheck =
        buildPhase:
        let

          buildPhaseFail = ''
            echo "Derivation won't build outside of a nix shell without submodules:"
            echo "  nix build '.?submodules=1#' # build from local sources"
            exit 1
          '';

        in
        if hasSubmodules then buildPhase else buildPhaseFail;
    };

  # Grab opam packages from opam file
  semgrepOpam = lib.buildOpamPkg {
    name = "semgrep";
    src = ./.;
    inputs = (
      with pkgs;
      [
        tree-sitter
      ]
      ++ (if pkgs.stdenv.isDarwin then [ libdwarf ] else [ ])
    );
  };

  devOptional = lib.buildOpamPkg {
    name = "optional";
    src = ./dev;
    # You can force versions of certain packages here
    query = {
      utop = "2.15.0";
    };
  };

  devRequired = lib.buildOpamPkg {
    name = "required";
    src = ./dev;
  };
in
let

  #
  # semgrep
  #
  env = {
    # Needed so we don't pass any flags in flags.sh
    SEMGREP_NIX_BUILD = "1";
  };
  semgrep = semgrepOpam.overrideAttrs (prev: rec {
    doNixSupport = false;
    # Special environment variables for osemgrep for linking stuff

    # coupling: if you add files here you probably want to add them to the
    # Dockerfile and the pro Dockerfile
    src = (
      lib.strictSrc ./. (
        with pkgs.lib.fileset;
        [
          ./Makefile
          ./cygwin-env.mk
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

          # only needed for testing
          # TODO split out into separate derivation
          ./cli/tests
          ./scripts/run-core-test
          ./scripts/make-symlinks
          ./test
          ./tests
        ]
      )
    );

    inherit env;

    buildPhase = lib.buildPhaseSubmoduleCheck "make core";
    # needed for networking tests
    nativeBuildInputs = (
      with pkgs;
      [
        cacert
        git
      ]
    );

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
  devPkgs = builtins.map lib.disableNixSupport (devOptional.buildInputs ++ devRequired.buildInputs);
in
{
  pkg = semgrep;
  devEnv = env;
  inherit devPkgs;
  inherit lib;
}
