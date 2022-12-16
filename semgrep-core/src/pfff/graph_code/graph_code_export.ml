(* Yoann Padioleau
 *
 * Copyright (C) 2018 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)

module G = Graph_code
module J = JSON

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let string_of_entity_kind kind =
  Entity_code.string_of_entity_kind kind

let json_short_of_node (str, kind) =
  J.Array [
    J.String str;
    J.String (string_of_entity_kind kind);
  ]

let json_of_node g (str, kind) =
  J.Object [
    "name_kind", json_short_of_node (str, kind);
    "location", J.String (
      try G.file_of_node (str, kind) g
      (* 'Dir' entities have no location for example *)
      with Not_found -> "UNKNOWN LOCATION"
    );
  ]


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let graph_to_json g =
  J.Object [
    "nodes", J.Array (
      G.all_nodes g |> List.map (json_of_node g)
    );
    "edges_Use", J.Array (
      G.all_use_edges g |> List.map (fun (src, dst) ->
        J.Object [
          "src", json_short_of_node src;
          "dst", json_short_of_node dst;
        ]
      )
    );
    "edges_Has", J.String "TODO";
  ]
