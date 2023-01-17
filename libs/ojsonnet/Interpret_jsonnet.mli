(* TODO: like for Manifest_jsonnet, we probably want at some point to return
 * a AST_generic.program instead of a JSON.t
 *)
val interpret : Common.filename -> JSON.t
