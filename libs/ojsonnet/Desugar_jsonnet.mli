(* Hook to customize how ojsonnet should resolve import expressions
 * (`local $NAME = import $PATH`). The first parameter below is the base
 * directory of the file currently processed and the second is the $PATH
 * above in the local expression.
 * The hook should return an AST_jsonnet expression if it can handle
 * the PATH or None, in which case it will default to a regular
 * jsonnet file import as in `local x = import "foo.jsonnet".
 *
 * This callback is useful for example in osemgrep to let ojsonnet
 * import yaml files (e.g., local x = import 'foo.yaml') or rules from the
 * registry (e.g., local x = import 'p/python').
 *)
type import_callback =
  string (* a directory *) -> string -> AST_jsonnet.expr option

val default_callback : import_callback

(* We pass the original file in addition to its AST so desugar can
 * handle correctly imports by using the dirname of the file as the
 * base directory for imports.
 * The use_std argument is set to true by default and means that
 * the program is first prefixed with 'local std = import "std.jsonnet"
 * where std.jsonnet is the content in Std_jsonnet.std.
 *
 * This function relies on the Conf_ojsonnet.use_std flag.
 *)
val desugar_program :
  ?import_callback:import_callback ->
  Fpath.t ->
  AST_jsonnet.program ->
  Core_jsonnet.program
