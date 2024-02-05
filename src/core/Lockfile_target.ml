(* A lockfile to be scanned by a supply chain rule.
   Should contain a list of all dependencies used by
   a project, at exact versions. Can be attached to a code target,
   or a target by itself.
*)
type t = {
  source : Source.t;
  file : Fpath.t;
  kind : Lockfile_kind.t;
  lazy_content : string lazy_t;
  lazy_ast_and_errors : Dependency.t list lazy_t;
  manifest : manifest_target option;
}

(* A manifest file, which should contain a list of
   direct dependencies (handwritten by a user).
   Used in the parsing of some lockfiles, to determine
   which dependencies are direct and which are transitive.
   Can only ever be attached to a lockfile target.
*)
and manifest_target = {
  source : Source.t;
  file : Fpath.t;
  kind : Manifest_kind.t;
  lazy_content : string lazy_t;
  lazy_ast_and_errors : Dependency.manifest_dependency list lazy_t;
}
