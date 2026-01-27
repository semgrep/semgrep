{
  pkgs,
  devAttrs ? [ ],
}:
let

  foldMapAttr =
    attr: attrs: (pkgs.lib.attrsets.mergeAttrsList (builtins.map (builtins.getAttr attr) attrs));
  foldMapList = attr: attrs: (builtins.concatMap (builtins.getAttr attr) attrs);
  foldMapSingle =
    attr: attrs: (builtins.foldl' (acc: attrs: [ (builtins.getAttr attr attrs) ] ++ acc) [ ] attrs);

  baseEnv = foldMapAttr "devEnv" devAttrs;
  baseBuildInputs = (foldMapList "devPkgs" devAttrs);
  baseInputsFrom = (foldMapSingle "pkg" devAttrs);

  extraInputs = (
    with pkgs;
    [
      pre-commit
      yq-go
    ]
  );
  extraInputsFrom = [ ];
  extraEnv = { };
in
rec {
  # no messing around! Just what you need, no hand holding, no custom shell
  pure = pkgs.mkShell {
    dontUseSetuptoolsShellHook = true;
    env = baseEnv // extraEnv;
    buildInputs = baseBuildInputs ++ extraInputs;
    inputsFrom = baseInputsFrom ++ extraInputsFrom;
    # https://pyproject-nix.github.io/uv2nix/usage/getting-started.html#setting-up-a-development-environment-optional
    # prevent side effects from PYTHONPATH set outside the shell/other nixpkgs
    # and set REPO_ROOT for editable python packages so the virtualenv knows where it is
    shellHook = ''
      unset PYTHONPATH
      export REPO_ROOT=$(git rev-parse --show-toplevel)/cli
    '';
  };
  # same as pure but we also set the shell back to the user's shell (direnv
  # does this already)
  default = pure.overrideAttrs (prev: {
    shellHook = prev.shellHook + ''
      # set to default user shell (such as /bin/zsh or /bin/fish)
      $(${pkgs.perl}/bin/perl -e '@x=getpwuid($<); print $x[8]')
      exit
    '';
  });
}
