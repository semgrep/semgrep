# you found our nix flake!
# use https://github.com/DeterminateSystems/nix-installer to get nix easily
# then run `nix develop` to get a shell with all the dependencies
# Quick start:
# To use your shell when developing:
# nix develop -c $SHELL
#
# To disallow all deps outside of nix:
# nix develop -i
#
# #TL;DR; Nix is a dependency manager. This makes it so it takes only one
# #command to have a fully functional correct and near identical development
# #environment across any OS, instead of the 10-20 commands it normally takes.
# #It's totally opt-in and will not impact anyones work flow.

# ## What is Nix?
#
# Nix is a package dependency manager for reproducible and correct builds. Nix
# is structured around the concept that each dependency is a function of its
# build dependencies. For example a hello world program takes in GCC and make as
# function arguments, and spits out a hello world binary. What makes this
# reproducible is that nix treats files as pointers, and produces a closure over
# all files in a build system. For example if the hello world depends on a C
# library, then your nix build of hello world won't complete unless you
# explicitly declare said library in your nix configuration. Since all build
# tools made with nix are built this way, nix knows exactly what any dependency
# needs to build, and to run, meaning *all builds are easily reproducible, and
# are not affected by what you may or may not have installed on your system*.

# ## Why should I care?
#
#Our OCaml code is (mostly) correct because that's a focus of the language
# itself. But it relies on a lot of C code and external dependencies, like
# `libev`, `libcurl`, `tree-sitter` etc. This means that if someone wants to
# build and contribute to Semgrep, they must install these dependencies, and
# hope that their versions is compatible. For example if you install the ocaml
# packages needed for osemgrep, and then install libev, things won't work, since
# the lwt package needs libev when its initially installed. If you're on mac,
# you also have to tell opam where libev is before installing lwt. So our OCaml
# is correct, but only if you can build it, and only if you build it with the
# right dependencies.

# By using nix, we can declare all these dependencies explicitly, and then
# anyone across any *nix system can easily build Semgrep with only one command!
# What's even better, is for regular contributors, these dependencies will auto
# update and rebuild whenever a new dependency is added. So if someone adds a
# dependency for lwt, libev will automatically be pulled in with 0 thought from
# the contributor, and no need to run `make dev-setup` or anything similar.
# Since all of the dependencies are reproducible, they're almost identical no
# matter what system you're on, so if someone runs into a bug in developing, you
# can have more confidence in eliminating a system difference as the source of
# the bug.

# Another bonus is that it's easy to know what version of a dependency you're
# on, e.g. what version of tree-sitter, or what version of OCaml. You can see
# how this also would be good for security :).

# There's a lot more places this can simplify things for us, like CI and
# releases. See future work for more details.

# Finally, nix configurations are written in a functional programming language.
# Don't you want to declare dependencies in a functional language?

# ## Who uses Nix?
#
# Not convinced? Here are some other notable OCaml projects using nix:

# - [dune](https://github.com/ocaml/dune)
# - [merlin](https://github.com/ocaml/merlin)
# - [ocaml-lsp](https://github.com/ocaml/ocaml-lsp)
# - [ocaml-re](https://github.com/ocaml/ocaml-re)

# There's also a ton of projects outside the OCaml world using nix too. Nix is
# battle tested, and has a huge community, with support for almost anything you
# can think of. There's even someone packaging Semgrep for nix.

# ## Try it out
#
# If you want to see what it's like to use nix, get a clean Linux or macOS box.
# Then [install nix](https://github.com/DeterminateSystems/nix-installer) with
# flake support. Finally run `nix develop`. Now you can run `make core` or any
# make targets in the CLI. That's one command to always be able to build Semgrep
# vs the 10-20 needed right now, if you're lucky, and then 2-3 every once in
# awhile to keep up to date.

# If you want to reproduce and help with a bug in someones branch, you can just
# run `nix develop` and `make test` to instantly have the same exact environment
# they ran into the bug in. If you're really unsure about if it's a system bug,
# you can run `nix develop -i` to exclude any non nix dependencies completely.

# If you don't use bash run `nix develop -c $SHELL` to get an environment with
# your shell.

# If you want to just build semgrep and run it you can do `nix run
# ".?submodules=1#"` for semgrep or `nix run ".?submodules=1#<target>"` where
# target is semgrep, pysemgrep, osemgrep, or semgrep-core.

# ## What's the catch?
#
# There's not a real big catch to using nix, except development time to set it
# up, which the PR that introduced this did. A few small catches are that nix can be a little
# slow on a first build, but no more than say `make dev-setup`, and that it uses
# a decent amount of storage space, but again, not much more than any other
# package manager.

# ## Contributing/Maintennance
#
# The maintenance for nix is super low. It automatically pulls in any dependency
# declared in our semgrep.opam file, which then pulls in any non ocaml
# dependencies those dependencies rely on. If someone adds a new build tool or
# python package, they will have to add the dependency to `flake.nix`. This
# means you have to read some comments then add the name of the package to an
# array somewhere, super easy. If you're interested in doing anything more
# complex, see the future reading section.

# # Future work
# ## Some issues with our current usage of Dune and Opam
#
# Right now our usage of dune is a little incorrect. In every language semgrep
# tree-sitter dune file (i.e. bash/tree-sitter/semgrep-bash), we don't pass the
# standard c/c++ flags to compile the C files. This means that the compiler
# provided by nix doesn't know if a c++ file is a c++ file, and fails to include
# the standard c++ library. There's an ongoing PR to fix this logic for nix, but
# really we should add ":standard" to the list of c++ flags in these dune files.
# There's a patch for now in the nix configuration but it's a bit ugly.

# Another issue with our opam usage is we don't declare dependencies on
# libraries provided by semgrep. I.e. we depend on commons, but don't declare
# that dependency in semgrep.opam. This breaks some autoresolving things in nix,
# which are patched for now, but would be nice if we fixed our usage of opam.

# ## CI testing
#
# If people like this tool, at some point it'd be great to add a CI workflow
# that ensures all PRs work even with nix. But let's see how this goes first.

# ## Improving our CI
#
# Where nix would smooth things over for us even more is simplifying our CI.
# Right now we have to guess and hope that our CI has the same dependencies as
# what's on our system, and it can be a fight to add any new dependencies, like
# libcurl. Not to mention our workflows differ by system, and it's a pain to
# make sure they all have the right package names. If we use nix in CI, all we
# need is to install nix on the target OS/architecture, then run nix build and
# it can run all build processes, and all e2e tests. Super simple workflow. It
# can even build the wheels, and static versions of Semgrep we'd want!

# # More reading
#
# Interested in reading more about nix? Here are some hand sources
#
# - [What is Nix](https://shopify.engineering/what-is-nix) - short intro to nix
#   by Shopify Engineering
#
# - [Nix Pills](https://nixos.org/guides/nix-pills/) -long form intro to nix
#
# - [Nix Flakes](https://zero-to-nix.com/concepts/flakes) - intro to the format
#   we use to configure nix
#
# - [opam-nix](https://www.tweag.io/blog/2023-02-16-opam-nix/) - how we nixify
#   opam deps
#
# - [Nix PhD Thesis](https://edolstra.github.io/pubs/phd-thesis.pdf) - Nix
#   creator's PhD thesis on Nix. ~275 pages but really approachable
{
  description = "Semgrep OSS is a fast, open-source, static analysis tool for searching code, finding bugs, and enforcing code standards at editor, commit, and CI time.";
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs, opam-repository }@inputs:
    let
      package = "semgrep";

    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        # TODO Use pkgsStatic if on linux
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        pythonPackages = pkgs.python310Packages;
        opamRepos = [ "${opam-repository}" ];
        # TODO split out osemgrep and pysemgrep into diff nix files
      in
      let

        # osemgrep/semgrep-core inputs
        osemgrepInputs = with pkgs; [
          tree-sitter
        ];
        devOpamPackagesQuery = {
          # You can add "development" ocaml packages here. They will get added
          # to the devShell automatically.
          ocaml-lsp-server = "*";
          utop = "*";
          ocamlformat = "*";
          earlybird = "*";
          merlin = "*";
        };
        opamQuery = devOpamPackagesQuery // {
          ## You can force versions of certain packages here
          # force the ocaml compiler to be 4.14.2 and from opam
          ocaml-base-compiler = "4.14.2";
          # needed for OCTS and isn't pulled in by semgrep.opam
          tsort = "*";
          # don't use bleeding edge cohttp
          cohttp-lwt = "5.3.0";
        };

        # repos = opamRepos to force newest version of opam
        scope = on.buildOpamProject' { repos = opamRepos; } ./. opamQuery;
        scopeOverlay = final: prev: {
          # You can add overrides here
          ${package} = prev.${package}.overrideAttrs (prev: {
            # Prevent the ocaml dependencies from leaking into dependent environments
            doNixSupport = false;
            # add tsort since it's not pulled in for whatever reason
            buildInputs = prev.buildInputs ++ [ final.tsort ];
          });
        };
        scope' = scope.overrideScope' scopeOverlay;

        # for development
        devOpamPackages = builtins.attrValues
          (pkgs.lib.getAttrs (builtins.attrNames devOpamPackagesQuery) scope');

        # osemgrep/semgrep-core
        # package with all opam deps but nothing else
        baseOpamPackage = scope'.${package}; # Packages from devPackagesQuery

        osemgrep = baseOpamPackage.overrideAttrs (prev: rec {
          pname = "osemgrep";
          buildInputs = prev.buildInputs ++ osemgrepInputs;
          # all the dune files of semgrep treesitter <LANG> are missing the
          # :standard field. Basically all compilers autodetct if something is c
          # or c++ based on file extension, and add the c stdlib based on that.
          # Nix doesn't because reasons:
          # https://github.com/NixOS/nixpkgs/issues/150655 Dune also passes
          # -xc++ if it detects a c++ file (again sane), but it's included in
          # the :standard var, which we don't add because ??? TODO add and
          # commit them instead of doing this
          buildPhase' = ''
            for f in $(find -type f -iname "dune");do
              substituteInPlace $f \
                --replace "flags -fPIC" "flags :standard -fPIC"
            done
            make core
          '';
          buildPhaseFail = ''
              echo "Derivation ${pname} won't build outside of a nix shell without submodules:"
              echo "  nix build '.?submodules=1#' # build from local sources"
              echo "  nix build '<uri>?submodules=1#' # build from remote sources"
              echo "  nix run '.?submodules=1#osemgrep' # run osemgrep from local sources"
              echo "  nix run '<uri>.?submodules=1#osemgrep' # run osemgrep from remote source"
              exit 1
          '';
          # make sure we have submodules
          # See https://github.com/NixOS/nix/pull/7862
          buildPhase = if self.submodules then osemgrep.buildPhase' else osemgrep.buildPhaseFail;
          # TODO check phase

          # DONE! Copy semgrep binaries!!!!
          installPhase = ''
            mkdir -p $out/bin
            cp _build/install/default/bin/* $out/bin
          '';

        });


        # pysemgrep inputs
        # coupling: anything added to pysemgrep for testing should be added here
        devPipInputs = with pythonPackages; [
          pkgs.git
          flaky
          pytest-snapshot
          pytest-mock
          pytest-freezegun
          types-freezegun
        ];

        # pysemgrep
        pysemgrep = with pythonPackages; buildPythonApplication {
          # thanks to @06kellyjac
          pname = "semgrep";
          inherit (osemgrep) version;
          src = ./cli;
          # TODO checks
          doCheck = false;

          # coupling: anything added to the pysemgrep setup.py should be added here
          propagatedBuildInputs = [
            attrs
            boltons
            colorama
            click
            click-option-group
            glom
            requests
            rich
            ruamel-yaml
            tqdm
            packaging
            jsonschema
            wcmatch
            peewee
            defusedxml
            urllib3
            typing-extensions
            tomli
          ];
          # doesn't work for some reason
          dontUseSetuptoolsShellHook = true;

          preFixup = ''
            makeWrapperArgs+=(--prefix PATH : ${osemgrep}/bin)
          '';
        };
        # TODO semgrep-js
      in
      {

        packages.osemgrep = osemgrep;
        packages.semgrep = pysemgrep;
        packages.default = pysemgrep;
        apps = {
          osemgrep = {
            type = "app";
            program = "${osemgrep}/bin/osemgrep";
          };
          semgrep-core = {
            type = "app";
            program = "${osemgrep}/bin/semgrep-core";
          };
          semgrep = {
            type = "app";
            program = "${pysemgrep}/bin/semgrep";
          };
          pysemgrep = {
            type = "app";
            program = "${pysemgrep}/bin/pysemgrep";
          };
          default = {
            type = "app";
            program = "${pysemgrep}/bin/semgrep";
          };
        };

        formatter = pkgs.nixpkgs-fmt;
        devShells.default = pkgs.mkShell {
          # See comment above osemgrep.buildPhase for why we need this
          # This doesnt work there because idk
          shellHook = with pkgs; ''
            export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -I${pkgs.libcxx.dev}/include/c++/v1"
          '';
          inputsFrom = [ osemgrep pysemgrep ];
          buildInputs = devOpamPackages ++ devPipInputs ++ (with pkgs; [
            pre-commit
            pipenv
            yq-go # for GHA workflows
          ]);
        };
      });
}
