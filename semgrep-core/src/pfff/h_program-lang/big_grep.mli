
type index = {
  big_string: string;
  pos_to_entity: (int, Database_code.entity) Hashtbl.t;
  case_sensitive: bool;
}
val empty_index: unit -> index

(* the list is supposed to be sorted by importance so that the
 * top n search returns first the most important entities
*)
val build_index:
  ?case_sensitive:bool ->
  Database_code.entity list -> index

val top_n_search:
  top_n:int ->
  query:string ->
  index ->
  Database_code.entity list

val naive_top_n_search:
  top_n:int ->
  query:string ->
  Database_code.entity list ->
  Database_code.entity list
