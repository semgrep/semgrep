(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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
open Common
module G = Graph_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Module to help visualize dependencies: is an entity an entry-point
 * of the program or one of its leaves. It also help visualize
 * all the errors in codegraph (lookup failures, unresolved method calls, etc).
 *
 * For the bottom up layer note that a file can be red and be shown
 * as used by a green. It's because This green file maybe use a green
 * entity of this red file, but not the very red entity of this red file.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let kind_of_rank ~max_total n =
  let percent = if max_total = 0 then 0 else Common2.pourcent n max_total in
  let percent_round = percent / 10 * 10 in
  spf "cover %d%%" percent_round

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let gen_rank_heatmap_layer g hentity_to_rank ~output =
  let group_by_file =
    hentity_to_rank |> Common.hash_to_list
    |> Common.map_filter (fun (node, v) ->
           try
             let file = G.file_of_node node g in
             (* we want to make sure this node has a line, some
              * E.File could be there because file_of_node works for them
              * but they have no line, so let's filter them here
              *)
             let _line = G.nodeinfo node g in
             Some (file, (node, v))
           with
           | Not_found -> None)
    |> Common.group_assoc_bykey_eff
  in
  let xs = hentity_to_rank |> Common.hash_to_list |> List.map snd in
  let max_total = Common2.maximum xs in

  let layer =
    {
      Layer_code.title = spf "Graph code rank (%s)" (Filename.basename output);
      description =
        "Associate a rank to each entity according to its depth\n\
         in the Use graph";
      files =
        group_by_file
        |> List.map (fun (file, nodes_and_rank) ->
               let max_file =
                 nodes_and_rank |> List.map snd |> Common2.maximum
               in

               ( file,
                 {
                   Layer_code.micro_level =
                     nodes_and_rank
                     |> List.map (fun (n, v) ->
                            let info = G.nodeinfo n g in
                            let line = info.Graph_code.pos.Parse_info.line in
                            (line, kind_of_rank v ~max_total));
                   macro_level = [ (kind_of_rank max_file ~max_total, 1.) ];
                 } ));
      kinds = Layer_code.heat_map_properties;
    }
  in
  Layer_code.save_layer layer output

let gen_statistics_layer ~root stats ~output =
  (* there is a priority order here, see simple_layer_of_parse_infos
   * that leverage this order
   *)
  let kinds =
    [
      ("lookup fail", "purple");
      ("unresolved calls", "red3");
      ("unresolved class access", "orange");
      ("unresolved method calls", "yellow");
      ("unresolved field access", "blue");
      ("resolved method calls", "green");
      ("resolved field access", "green3");
    ]
  in
  let pre b s = if b then "resolved " ^ s else "unresolved " ^ s in

  let infos =
    (!(stats.G.unresolved_calls) |> List.map (fun x -> (x, "unresolved calls")))
    @ (!(stats.G.unresolved_class_access)
      |> List.map (fun x -> (x, "unresolved class access")))
    @ (!(stats.G.field_access)
      |> List.map (fun (x, b) -> (x, pre b "field access")))
    @ (!(stats.G.method_calls)
      |> List.map (fun (x, b) -> (x, pre b "method calls")))
    @ (!(stats.G.lookup_fail)
      |> List.map (fun (x, (_str, _kind)) -> (x, "lookup fail")))
    @ []
  in
  let layer =
    Layer_code.simple_layer_of_parse_infos ~root
      ~title:"Graph code error statistics" ~description:"" infos kinds
  in
  Layer_code.save_layer layer output;
  ()

(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let actions () =
  [
    ( "-gen_bottomup_layer",
      " <graph_file> <output>",
      Common.mk_action_2_arg (fun graph_file _output ->
          let g = G.load graph_file in
          let hrank = G.bottom_up_numbering g in
          let d, _, _ = Common2.dbe_of_filename graph_file in
          let output =
            Common2.filename_of_dbe (d, "layer_graph_code", "json")
          in
          gen_rank_heatmap_layer g hrank output) );
  ]
