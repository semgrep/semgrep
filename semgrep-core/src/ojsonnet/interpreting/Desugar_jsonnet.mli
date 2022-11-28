(* Hook to customize how ojsonnet should resolve import expressions
 * (`local $NAME = $PATH`). The first parameter below is the base directory
 * of the file currently processed and the second is the $PATH above in
 * the local expression.
 * The hook should return an AST_jsonnet expression if it can handle
 * the PATH or None, in which case it will default to a regular
 * jsonnet file import as in `local x = "foo.jsonnet".
 *
 * This callback is useful for example in osemgrep to let ojsonnet
 * import yaml files (e.g., local x = 'foo.yaml') or rules from the
 * registry (e.g., local x = 'p/python').
 *)
type import_callback = Common.dirname -> string -> AST_jsonnet.expr option

val default_callback : import_callback

(* We pass the original file in addition to its AST so desugar can
 * handle correctly imports and import from the dirname of the file.
 *)
val desugar_program :
  ?import_callback:import_callback ->
  Common.filename ->
  AST_jsonnet.program ->
  Core_jsonnet.program
