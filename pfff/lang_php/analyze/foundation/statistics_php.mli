(*s: statistics_php.mli *)
(*x: statistics_php.mli *)
type stat = (string, int) Common2.hash_with_default

type stat_hooks = {
  entity: (Entity_code.entity_kind * string) -> unit;
  (* the second node contains partial information, such as only
   * the method
   *)
  call: (Callgraph_php2.node * Callgraph_php2.node) -> unit;
}
(* works by side effect on stat2 hash *)
val stat_of_program: 
  ?hooks:stat_hooks -> stat -> Common.filename -> Cst_php.program -> unit


type stat2 = {
  mutable functions: int;
  mutable classes: int;
  mutable toplevels_funcalls: int;
  mutable toplevels_assign: int;
  mutable toplevels_other: int;
  mutable toplevels_include_requires: int;

  mutable toplevels_funcalls_kinds: (string * int) list;
  (* toplevels_assign_kinds? *)
}
type php_file_kind =
  | LibFile
  | IncluderFile
  | ScriptOrEndpointFile

val default_stat2: unit -> stat2
val string_of_stat: stat2 -> string
val stat2_of_program: Cst_php.program -> stat2


(* helpers *)
val kind_of_file_using_stat: stat -> php_file_kind

(*e: statistics_php.mli *)
