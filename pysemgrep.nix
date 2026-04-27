{
  src ? ./cli,
}:
{
  pkgs,
  semgrep,
  uv2nix,
  pyproject-nix,
  pyproject-build-systems,
}:
# We use uv2nix to do python nix stuff
# https://pyproject-nix.github.io/uv2nix/usage/getting-started.html
let
  python = pkgs.python3;
  inherit (pkgs.callPackages pyproject-nix.build.util { }) mkApplication;

  workspace = uv2nix.lib.workspace.loadWorkspace { workspaceRoot = src; };
  # overlay of all python packages that are semgrep dependencies
  pythonOverlay = workspace.mkPyprojectOverlay {
    sourcePreference = "wheel";
  };
  # Editable for dev environment, so we can edit the source code directly
  # and still have the cli command semgrep work
  editableOverlay = workspace.mkEditablePyprojectOverlay {
    root = "$REPO_ROOT";
  };
  # generate the python packages, one is
  pythonSet =
    (pkgs.callPackage pyproject-nix.build.packages {
      inherit python;
    }).overrideScope
      (
        pkgs.lib.composeManyExtensions [
          pyproject-build-systems.overlays.wheel
          pythonOverlay
        ]
      );
  venv = pythonSet.mkVirtualEnv "pysemgrep-env" workspace.deps.default;

  devPythonSet = pythonSet.overrideScope editableOverlay;
  devVenv = devPythonSet.mkVirtualEnv "pysemgrep-dev-env" workspace.deps.all;

  devPkgs = [
    devVenv
    pkgs.uv
  ];
  devEnv = {
    UV_NO_SYNC = "1"; # nix manages the packages so don't let uv do it
    UV_PYTHON_DOWNLOADS = "never"; # don't let uv manage downloads
    UV_PYTHON = devPythonSet.python.interpreter; # force uv to use our python
    VIRTUAL_ENV = devVenv; # for type checkers
  };

  pysemgrep =
    (mkApplication {
      inherit venv;
      package = pythonSet.semgrep;
    }).overrideAttrs
      (old: {
        nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.makeBinaryWrapper ];
        # add ${semgrep}/bin to PATH so semgrep can find its ocaml binaries
        postInstall = ''
          wrapProgram $out/bin/semgrep \
            --prefix PATH : ${pkgs.lib.makeBinPath [ semgrep ]}
          wrapProgram $out/bin/pysemgrep \
            --prefix PATH : ${pkgs.lib.makeBinPath [ semgrep ]}
        '';
      });
in
{
  pkg = pysemgrep;
  inherit devEnv devPkgs;
}
