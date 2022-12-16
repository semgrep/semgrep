
type fact =
  | At of entity * Common.filename (* readable path *) * int (* line *)
  | Kind of entity * Entity_code.entity_kind
  | Type of entity * string

  | Extends of string * string
  | Implements of string * string
  | Mixins of string * string

  | Privacy of entity * Entity_code.privacy

  | Call of entity * entity
  | UseData of entity * entity * bool option (* read/write *)
  | Special of entity * entity * entity * string (* field/function *)

  | Misc of string

and entity =
  string list (* package/module/namespace/class qualifier*) * string (* name *)

val string_of_fact: fact -> string
val entity_of_str: string -> entity

(* reused in other modules which generate prolog facts *)
val string_of_entity_kind: Entity_code.entity_kind -> string
