(* TODO: like for Manifest_jsonnet, we probably want at some point to return
 * a AST_generic.program instead of a JSON.t
 * TODO: not implemented currently.
 *)
val interpret : Fpath.t -> JSON.t
