
type outline = outline_node Common2.tree2
and outline_node = {
  stars : string;
  title : string;
  before_first_children : string list;
}



val outline_default_regexp : string

(* value for implicit root *)
val root_title : string
val root_stars : string
val is_root_node : outline_node -> bool


val parse_outline : ?outline_regexp:string -> Common.filename -> outline
val write_outline : outline -> Common.filename -> unit
