(* eXtended target.
 *
 * This type is mostly used in the engine, to pass around extra information
 * associated to each target.
 *
 * related: Input_to_core.target, which is what is passed
 * to semgrep-core via -target.
 *)

type t = {
  file : Fpath.t;
  xlang : Xlang.t;
  lazy_content : string lazy_t;
  (* This is valid only for xlang = Xlang.L ..., not for LRegex|LGeneric *)
  lazy_ast_and_errors : (AST_generic.program * Tok.location list) lazy_t;
  lockfile_data : lockfile_data option;
}

and lockfile_data = {
  lockfile : Fpath.t;
  ecosystem : Supply_chain.ecosystem;
  lazy_lockfile_content : string lazy_t;
  (* TODO: parsed lockfile type  *)
  lazy_lockfile_ast_and_errors : Supply_chain.dependency list lazy_t;
}
