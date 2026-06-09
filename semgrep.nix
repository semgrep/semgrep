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

      # Build an opam-nix `query` (a `{ name = version; }` attrset of exact
      # version pins) from a `*.opam.locked` lockfile.
      #
      # opam-nix's `buildOpamProject` ignores lockfiles entirely: it reads the
      # unlocked `.opam` file and re-solves the whole dependency graph against
      # `opam-repository` from scratch. Unconstrained deps (e.g. `pbrt`, whose
      # version is only pulled in transitively by `ocaml-protoc {>= "3.1.1"}`)
      # are then free to resolve to whatever the repo offers, which is how we
      # ended up with two incompatible `pbrt` versions in one build. Feeding
      # the lockfile's pins back in as the query forces the solver to the exact
      # locked versions, so the resolved set matches `opam install --locked`.
      #
      # We parse the lockfile with opam-nix's `fromOpam` (which shells out to
      # `opam2json`, so this is import-from-derivation — same as the solver the
      # rest of buildOpamProject already relies on) and keep only deps pinned
      # with an exact `{= "<version>"}` constraint. `eq` constraints can sit
      # either directly in a dep's `conditions` list or nested as the `lhs` of
      # an `and` (e.g. `{= "3.19.1" & build}`); we look in both places. Deps
      # without an `eq` pin (none in our lockfiles) are simply left for the
      # solver. `.dev` versions correspond to the git `pin-depends` already
      # declared in the unlocked `.opam`, and opam-nix resolves those from the
      # pinned source.
      lockfileQuery =
        lockfile:
        let
          # opam2json renders a dep constraint either as a single condition
          # object or, for `a & b`, as an `and` node with `lhs`/`rhs`. Find the
          # `eq` relop wherever it lives.
          eqVersion =
            cond:
            if cond ? prefix_relop && cond.prefix_relop == "eq" then
              cond.arg
            else if cond ? logop && cond.logop == "and" then
              # only the lhs of our `{= "v" & build}`-style pins carries the
              # version; recurse into both sides to be safe.
              let
                l = eqVersion cond.lhs;
              in
              if l != null then l else eqVersion cond.rhs
            else
              null;
          # Each `depends` entry is either a bare string (no constraint) or
          # `{ val = name; conditions = [ ... ]; }`.
          depToPin =
            dep:
            if builtins.isAttrs dep && dep ? val && dep ? conditions then
              let
                versions = builtins.filter (v: v != null) (map eqVersion dep.conditions);
              in
              if versions == [ ] then { } else { ${dep.val} = builtins.head versions; }
            else
              { };
          parsed = on.fromOpam (builtins.readFile lockfile);
          depends = parsed.depends or [ ];
        in
        builtins.foldl' (acc: dep: acc // (depToPin dep)) { } depends;

      # Resolve the committed, platform-specific lockfile for this build.
      #
      # The lockfiles are platform dependent (see opam-lockfiles/README.md), so
      # the repo commits one variant per platform under `<dir>/<name>.opam.<os>-
      # <arch>.locked` and a `pick-lockfile.sh` script copies the matching one
      # to `<name>.opam.locked` at (non-nix) build time. That picked file is
      # gitignored, so it isn't part of the flake source and we can't read it
      # here — instead we select the committed variant directly from the Nix
      # `system`, mirroring pick-lockfile.sh's os/arch mapping.
      lockfileFor =
        dir: name:
        let
          suffix =
            {
              "aarch64-darwin" = "mac-arm64";
              "x86_64-darwin" = "mac-x86";
              "aarch64-linux" = "linux-arm64";
              "x86_64-linux" = "linux-amd64";
            }
            .${system}
              or (throw "no committed ${name} lockfile for nix system ${system}");
        in
        # path + string keeps this a source-relative path (so `readFile` reads
        # the committed file) rather than coercing `dir` into the store first.
        dir + "/${name}.opam.${suffix}.locked";

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
          # Optional path to a `*.opam.locked` lockfile. When set, every exact
          # `{= "v"}` pin in it is fed into the solver query so the build
          # resolves to the locked versions instead of re-solving freely. See
          # `lockfileQuery`.
          lockfile ? null,
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
          # Lockfile pins are the floor; the explicit `query` and `baseQuery`
          # win on conflict so callers can still override and the flambda
          # option is preserved.
          lockedQuery = if lockfile == null then { } else lockfileQuery lockfile;
          resolveArgs = {
            # speeds up so we don't get a solver timeout
            criteria = null;
            # opam-nix's buildOpamProject defaults `dev = true`, which causes
            # the solver to treat every `:dev`-filtered dep of every package
            # in the graph as required. Outside nix, the opam `:dev` filter
            # only activates when the *installed* package was sourced from
            # a git pin, and `opam install --locked` evaluates it to false
            # for all our transitive deps — so setting this to false here
            # matches the non-nix behavior. Notably, this prevents opam-nix
            # from pulling in timedesc's `bisect_ppx {dev & >= 2.5.0}` dep,
            # which would otherwise cap cmdliner < 2.0.0.
            dev = false;
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
          } name src (lockedQuery // baseQuery // query);
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
    lockfile = lib.lockfileFor ./opam-lockfiles "semgrep";
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
