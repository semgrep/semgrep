(* will invoke 'dot' to generate the PS/PDF/PNG of the graph and
 * depending on the platform will then invoke 'gv' or 'open'.
 *)
open Ograph_extended

val print_ograph_mutable_generic :
  ?title:string option ->
  (* label for the entire graph *)
  ?display_graph:bool ->
  ?output_file:string (* filename *) ->
  (* what string to print for a node and how to color it *)
  s_of_node:(nodei * 'node -> string * string option * string option) ->
  ('node, 'edge) ograph_mutable ->
  unit

val print_ograph_extended :
  ('node * string, 'edge) ograph_extended ->
  string (* output file *) ->
  bool (* launch gv / show png `? *) ->
  unit

val print_ograph_mutable :
  ('node * string, 'edge) ograph_mutable ->
  string (* output file *) ->
  bool (* launch gv / show png ? *) ->
  unit

val launch_gv_cmd : string (* filename *) -> unit
