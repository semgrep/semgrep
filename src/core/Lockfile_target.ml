type t = {
  lockfile : Fpath.t;
  ecosystem : Dependency.ecosystem;
  lazy_lockfile_content : string lazy_t;
  lazy_lockfile_ast_and_errors : Dependency.t list lazy_t;
  manifest_target : manifest_target option;
}

and manifest_target = {
  manifest : Fpath.t;
  lazy_manifest_content : string lazy_t;
  lazy_manifest_ast_and_errors : Dependency.manifest_dependency list lazy_t;
}
