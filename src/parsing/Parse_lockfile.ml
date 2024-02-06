module In = Input_to_core_t

let parse_lockfile :
    In.lockfile_kind ->
    Lockfile_target.manifest_target option ->
    Fpath.t ->
    Dependency.t list = function
  (* TODO: add parsers, guard behind semgrep-pro  *)
  | PackageLockJsonV3 -> fun _ _ -> []

let parse_manifest :
    In.manifest_kind -> Fpath.t -> Dependency.manifest_dependency list =
  function
  (* TODO: add parsers, guard behind semgrep-pro  *)
  | PackageJson -> fun _ -> []
