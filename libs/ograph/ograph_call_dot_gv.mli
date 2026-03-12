(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* The functions below invoke external commands:
 * - 'dot' to generate the PS/PDF/PNG of the graph
 * - depending on the platform 'gv' or 'open' to visualize the PDF
 * - 'uname' to decide the platform
 *)
open Ograph_extended

val print_ograph_mutable_generic :
  ?title:string ->
  (* label for the entire graph *)
  ?display_graph:bool ->
  ?output_file:Fpath.t ->
  (* what string to print for a node and how to color it *)
  s_of_node:(nodei * 'node -> string * string option * string option) ->
  ('node, 'edge) ograph_mutable ->
  unit

val pp_ograph_mutable_generic :
  ?title:string ->
  s_of_node:(nodei * 'node -> string * string option * string option) ->
  Format.formatter ->
  ('node, 'edge) ograph_mutable ->
  unit

val print_ograph_mutable :
  ('node * string, 'edge) ograph_mutable ->
  string (* output file *) ->
  bool (* launch gv / show png ? *) ->
  unit

val launch_gv_cmd : string (* filename *) -> unit
