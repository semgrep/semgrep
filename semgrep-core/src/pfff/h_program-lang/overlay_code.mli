
type overlay = {
  (* the filenames are in a readable path format *)
  orig_to_overlay: (Common.filename, Common.filename) Hashtbl.t;
  overlay_to_orig: (Common.filename, Common.filename) Hashtbl.t;
  data: (Common.filename (* overlay *) * Common.filename) list;

  (* in realpath format *)
  root_orig: Common.dirname;
  root_overlay: Common.dirname;
}
val overlay_equivalences:
  dir_orig:Common.dirname -> dir_overlay:Common.dirname ->
  overlay

val adapt_layer:
  Layer_code.layer -> overlay -> Layer_code.layer

val adapt_database:
  Database_code.database -> overlay -> Database_code.database

val load_overlay: Common.filename -> overlay
val save_overlay: overlay -> Common.filename -> unit


val check_overlay:
  dir_orig:Common.dirname -> dir_overlay:Common.dirname -> unit

val gen_overlay:
  dir_orig:Common.dirname -> dir_overlay:Common.dirname ->
  output:Common.filename -> unit

val adapt_layers:
  overlay:overlay ->
  dir_layers_orig:Common.dirname -> dir_layers_overlay:Common.dirname ->
  unit
