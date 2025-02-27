{ pkgs, devAttrs ? [ ] }:
let

  foldMapAttr = attr: attrs:
    (pkgs.lib.attrsets.mergeAttrsList
      (builtins.map (builtins.getAttr attr) attrs));
  foldMapList = attr: attrs: (builtins.concatMap (builtins.getAttr attr) attrs);
  foldMapSingle = attr: attrs:
    (builtins.foldl' (acc: attrs: [ (builtins.getAttr attr attrs) ] ++ acc) [ ]
      attrs);

  baseEnv = foldMapAttr "devEnv" devAttrs;
  baseBuildInputs = (foldMapList "devPkgs" devAttrs);
  baseInputsFrom = (foldMapSingle "pkg" devAttrs);

  extraInputs = (with pkgs; [ pipenv pre-commit yq-go ]);
  extraInputsFrom = [ ];
  extraEnv = { };
in rec {
  # no messing around! Just what you need, no hand holding
  pure = pkgs.mkShell {
    dontUseSetuptoolsShellHook = true;
    env = baseEnv // extraEnv;
    buildInputs = baseBuildInputs ++ extraInputs;
    inputsFrom = baseInputsFrom ++ extraInputsFrom;
    shellHook = "";
  };
  # same as pure but does some pipenv stuff to make semgrep nicer
  direnv = pure.overrideAttrs (prev: {
    shellHook = prev.shellHook + ''
      # setup pipenv for people automatically
      source ${./scripts/add-pysemgrep-to-path.sh}
    '';
  });
  # same as direnv but we also set the shell back to the user's shell (direnv
  # does this already)
  default = direnv.overrideAttrs (prev: {
    shellHook = prev.shellHook + ''
      # set to default user shell (such as /bin/zsh or /bin/fish)
      $(${pkgs.perl}/bin/perl -e '@x=getpwuid($<); print $x[8]')
      exit
    '';
  });
}
