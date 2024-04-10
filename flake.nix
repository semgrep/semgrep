# you found our nix flake!
# use https://github.com/DeterminateSystems/nix-installer to get nix easily
# then run `nix develop` to get a shell with all the dependencies
#
# To use your shell when developing:
# nix develop -c $SHELL
#
# To disallow all deps outside of nix:
# nix develop -i
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
          # You can add "development" ocaml packages here. They will get added to the devShell automatically.
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
          # all the dune files of semgrep treesitter <LANG>
          # are missing the :standard field. Basically all compilers autodetct if something
          # is c or c++ based on file extension, and add the c stdlib based on that. Nix doesn't
          # because reasons: https://github.com/NixOS/nixpkgs/issues/150655
          # Dune also passes -xc++ if it detects a c++ file (again sane), but it's included
          # in the :standard var, which we don't add because ???
          # TODO add and commit them instead of doing this
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
          default = self.outputs.${system}.apps.semgrep;
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
            pipenv
          ]);
        };
      });
}
