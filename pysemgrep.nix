{
  src ? ./cli,
}:
{ pkgs, semgrep }:
let
  pythonPkgs = pkgs.python312Packages;

  # TODO add opentelemetry-instrumentation-threading as a nix package
  # to get working
  # We also probably want to switch to uv since this approach is fragile
  #
  # For now we can just use pipenv
  # pysemgrep inputs pulled from pipfile
  # pydepsFromPipfile =
  #   setupPy: pipfile: type:
  #   let
  #     pipfileLockInputs'' = with builtins; (attrNames ((fromJSON (readFile (pipfile))).${type}));
  #     # remove semgrep from the lockfile inputs
  #     pipfileLockInputs' = pkgs.lib.lists.remove "semgrep" pipfileLockInputs'';

  #     setupPyFile = (builtins.readFile setupPy);
  #     # check if the package is in the setup.py before adding it to the list
  #     isInSetupPy = name: (builtins.match ".*${name}.*" setupPyFile) != null;
  #     # filter out Windows-specific packages that aren't available in Nix
  #     isNotWindowsOnly = name: !(builtins.elem name [ "pywin32" ]);
  #     pipfileLockInputs = builtins.filter (
  #       name: isInSetupPy name && isNotWindowsOnly name
  #     ) pipfileLockInputs';
  #     # replace . with -
  #   in
  #   builtins.map (name: builtins.replaceStrings [ "." ] [ "-" ] name) pipfileLockInputs;

  # pipfile = src + "/Pipfile.lock";
  # setupPy = src + "/setup.py";
  # pythonInputs = builtins.map (name: pythonPkgs.${name})
  #   (pydepsFromPipfile setupPy pipfile "default");

  # devPythonInputs = builtins.map (name: pythonPkgs.${name})
  #  ((pydepsFromPipfile pipfile "develop"));

  devPkgs = [ pkgs.pipenv ];

  pysemgrep = pythonPkgs.buildPythonApplication {
    # thanks to @06kellyjac
    pname = "pysemgrep";
    inherit (semgrep) version;
    inherit src;

    pyproject = true;
    build-system = [ pythonPkgs.setuptools ];

    propagatedBuildInputs = [ semgrep ];
    # Stops weird long step when entering shell
    dontUseSetuptoolsShellHook = true;
  };
in
{
  pkg = pysemgrep;
  devEnv = { };
  inherit devPkgs;
}
