(*s: treemap_json.mli *)

(*s: signature treemap_of_json *)
val treemap_of_json:
  Json_type.json_type ->
  (Common.dirname, Common.filename * int) Treemap.treemap
(*e: signature treemap_of_json *)

(*s: signature json_of_treemap *)
val json_of_treemap:
  ('dir, 'file) Treemap.treemap -> Json_type.json_type
(*e: signature json_of_treemap *)

val json_of_treemap_rendering:
  Treemap.treemap_rendering -> Json_type.json_type

val actions : unit -> Common.cmdline_actions

(*e: treemap_json.mli *)
