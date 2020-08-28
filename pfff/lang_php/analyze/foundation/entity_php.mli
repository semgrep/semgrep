(*s: entity_php.mli *)

(* Being able to access the definition of a class from a a program requires
 * a global analysis to find where is the class. This should mean
 * that each time some of the analyze_php/ functions need such thing they
 * would need to know about database_php.ml which leads to too many
 * dependencies. Enter 'entity_finder', which is usually build
 * via a closure from a database, but which hides the database to the
 * function using it. See database_php_build.build_entity_finder
 * or entity_finder_of_graph_code below for functions building
 * such entity_finder.
 * 
 * For methods the string below will also contain the class as
 * in "Foo::method".
 *)
type id_kind = Entity_code.entity_kind
type entity_finder = (id_kind * string) -> Cst_php.entity list

(* note: use global hcache_entities, so may need to reset it *)
val entity_finder_of_graph_code: 
  ?check_dupes:bool ->
  Graph_code.graph -> 
  Common.dirname -> entity_finder
(* use the Hashtbl.find_all property *)
(* val hcache_entities: (string * id_kind, Cst_php.entity) Hashtbl.t *)

type method_identifier = (string * string)
(*
val string_of_method_identifier: method_identifier -> string
val method_identifier_of_string: string -> method_identifier
*)

type id = Id of int

type fullid = filepos
 and filepos = {
  file: Common.filename;
  line: int;
  column: int;
}

val str_of_id: id -> string
val str_of_fullid: fullid -> string

val fullid_regexp: string
val fullid_of_string: string -> fullid

val filepos_of_parse_info: Parse_info.token_location -> filepos

val string_of_id_kind: id_kind -> string

val vof_filepos: filepos -> OCaml.v
(*x: entity_php.mli *)
(*e: entity_php.mli *)
