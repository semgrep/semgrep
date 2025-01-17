{ pkgs, devAttrs ? [ ], extraInputs ? [ ] }:
let
  foldMapAttr = attr: attrs:
    (pkgs.lib.attrsets.mergeAttrsList
      (builtins.map (builtins.getAttr attr) attrs));
  foldMapList = attr: attrs: (builtins.concatMap (builtins.getAttr attr) attrs);
  foldMapSingle = attr: attrs:
    (builtins.foldl' (acc: attrs: [ (builtins.getAttr attr attrs) ] ++ acc) [ ]
      attrs);
  env = foldMapAttr "devEnv" devAttrs;
  buildInputs = (foldMapList "devPkgs" devAttrs) ++ extraInputs;
  inputsFrom = foldMapSingle "pkg" devAttrs;
in pkgs.mkShell {
  inherit env inputsFrom buildInputs;

}
