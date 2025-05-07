(* We need cap.exec because the functions below will invoke
 * - 'dot' to generate the PS/PDF/PNG of the graph
 * - depending on the platform 'gv' or 'open' to visualize the PDF
 * - 'uname' to decide of the platform
 *)
open Ograph_extended

(* TODO: not sure why but I can't make it < Cap.exec; ..> below *)
val print_ograph_mutable_generic :
  < Cap.exec > ->
  ?title:string ->
  (* label for the entire graph *)
  ?display_graph:bool ->
  ?output_file:Fpath.t ->
  (* what string to print for a node and how to color it *)
  s_of_node:(nodei * 'node -> string * string option * string option) ->
  ('node, 'edge) ograph_mutable ->
  unit

val pp_ograph_mutable_generic :
  < Cap.exec ; Cap.tmp ; .. > ->
  ?title:string ->
  s_of_node:(nodei * 'node -> string * string option * string option) ->
  Format.formatter ->
  ('node, 'edge) ograph_mutable ->
  unit

val print_ograph_mutable :
  < Cap.exec > ->
  ('node * string, 'edge) ograph_mutable ->
  string (* output file *) ->
  bool (* launch gv / show png ? *) ->
  unit

val launch_gv_cmd : < Cap.exec ; .. > -> string (* filename *) -> unit
