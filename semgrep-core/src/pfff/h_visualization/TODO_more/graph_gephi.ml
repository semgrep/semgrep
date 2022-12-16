(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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

module G = Graph
open Xml_types


(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Wrappers to use Gephi (http://gephi.org/), to generate data
 * in its GEFX format.
 *)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(* see http://gexf.net/format/ *)
let graph_to_gefx ~str_of_node ~output ~tree ~weight_edges g =
  Common.with_open_outfile output (fun (pr_no_nl, _chan) ->
    let nodes = G.nodes g in
(*
    let x = ref 0 in
    let hclass = Hashtbl.create 101 in
*)

    let nodes_xml =
      match tree with
      | None ->
          nodes |> List.map (fun n ->
            let modularity_class = "nothing"
(*
              let s = str_of_node n in
              let xs = Common.split "/" s in
              let str_class = List.hd xs in
              str_class
*)
(*
              if Hashtbl.mem hclass str_class
              then Hashtbl.find hclass str_class
              else begin
                incr x;
                Hashtbl.add hclass str_class !x;
                !x
              end
*)
            in
            Element ("node", [
              "id", i_to_s (G.ivertex n g);
              "label", str_of_node n;
            ], [Element ("attvalues", [], [
              Element("attvalue", [
                "for", "modularity_class";
                "value", modularity_class;
              ], [])
            ])
            ])
          )
      | Some tree ->
          (* see: http://gexf.net/format/hierarchy.html *)
          let rec aux tree =
            match tree with
            | Common2.Leaf f ->
                Element ("node", [
                  "id", i_to_s (G.ivertex f g);
                  "label", str_of_node f;
                ], [])
            | Common2.Node (dir, xs) ->
                let children = List.map aux xs in
                Element ("node", [
                  "id", i_to_s (G.ivertex dir g);
                  "label", String.uppercase (str_of_node dir) ^ "/";
                ], [

                           Element ("nodes", [], children);
                         ])
          in
          [aux tree]
    in
    let edges_xml = nodes |> List.map (fun n ->
      let succ = G.succ n g in
      succ |> Common.map_filter (fun n2 ->
        let weight =
          match weight_edges with
          | None -> 1.
          | Some h ->
              Hashtbl.find h (n, n2)
        in
        if weight = 0.
        then None
        else Some (
          Element ("edge", [
            "source", i_to_s (G.ivertex n g);
            "target", i_to_s (G.ivertex n2 g);
            "weight", spf "%5.1f" weight;
          ], [])
        )
      )) |> List.flatten
    in
    let xml =
      Element (
        "gexf", [
          "xmlns", "http://www.gexf.net/1.2draft";
          "version", "1.2";
        ], [
          Element (
            "meta", [
              "lastmodifieddate", "2011-03-20";
            ], [
              Element ("creator", [], [PCData "pfff"]);
              Element ("description", [], [PCData "yep"]);
            ]);
          Element (
            "graph", [
              "mode", "static";
              "defaultedgetype", "directed";
            ], [
              Element ("attributes", [
                "class", "node";
                "mode", "static";
              ], [
                         Element("attribute", [
                           "id", "modularity_class";
                           "title", "Modularity Class";
                           "type", "string";
                         ], [])
                       ]);
              Element ("nodes", [], nodes_xml);
              Element ("edges", [], edges_xml);
            ]
          )
        ]
      )
    in
    let s = Xml_parse.to_string_fmt xml in
    pr_no_nl s
  )
