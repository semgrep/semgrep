
type xhprof_call = {
  src: xhprof_entity;
  dest: xhprof_entity;
}
 and xhprof_entity =
  | Function of string
  | Method of string (* class *) * string (* method *)
  | Main
  | PruneChild
  | RunInit of Common.filename
  | Misc of string

val string_of_xhprof_entity: xhprof_entity -> string
val parse_xhprof_entity_string: string -> xhprof_entity

val parse_caller_callee_string: string -> xhprof_call

(* some function or method names have a @ suffix. not sure why *)
val remove_at_suffix: string -> string
