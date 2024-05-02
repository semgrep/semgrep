(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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
open Fpath_.Operators
module E = Entity_code
module G = Graph_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* we sometimes want to collapse unimportant directories under a "..."
 * fake intermediate directory. So one can create an adjust file with
 * for instance:
 *   api -> extra/
 * and we will delete the current parent of 'api' and relink it to the
 * extra/ entity (possibly newly created)
 *)
type adjust = string * string

(* skip certain edges that are marked as ok regarding backward dependencies *)
type dependency = Graph_code.node * Graph_code.node
type whitelist = dependency list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let node_of_string s =
  if s =~ "\\([^:]*\\):\\(.*\\)" then
    let s1, s2 = Common.matched2 s in
    (s2, E.entity_kind_of_string s1)
  else failwith (spf "node_of_string: wrong format '%s'" s)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let load_adjust file =
  UFile.cat file
  |> List_.exclude (fun s -> s =~ "#.*" || s =~ "^[ \t]*$")
  |> List.map (fun s ->
         match s with
         | _ when s =~ "\\([^ ]+\\)[ ]+->[ ]*\\([^ ]+\\)" -> Common.matched2 s
         | _ -> failwith ("wrong line format in adjust file: " ^ s))

let load_whitelist file =
  UFile.cat file
  |> List.map (fun s ->
         if s =~ "\\(.*\\) --> \\(.*\\) " then
           let s1, s2 = Common.matched2 s in
           (node_of_string s1, node_of_string s2)
         else failwith (spf "load_whitelist: wrong line: %s" s))

let save_whitelist xs file g =
  UFile.with_open_out file (fun (pr_no_nl, _chan) ->
      xs
      |> List.iter (fun (n1, n2) ->
             let file = G.file_of_node n2 g in
             pr_no_nl
               (spf "%s --> %s (%s)\n" (G.string_of_node n1)
                  (G.string_of_node n2) !!file)))

(* Used mainly to collapse many entries under a "..." intermediate fake
 * parent. Maybe this could be done automatically in codegraph at some point,
 * like ndepend does I think.
 *)
let adjust_graph g xs whitelist =
  let mapping = Hashtbl.create 101 in
  g |> G.iter_nodes (fun (s, kind) -> Hashtbl_.push mapping s (s, kind));
  xs
  |> List.iter (fun (s1, s2) ->
         let nodes = Hashtbl_.get_stack mapping s1 in

         let new_parent = (s2, E.Dir) in
         G.create_intermediate_directories_if_not_present g s2;
         match nodes with
         | [ n ] ->
             let old_parent = G.parent n g in
             G.remove_edge (old_parent, n) Has g;
             G.add_edge (new_parent, n) Has g
         | [] -> failwith (spf "could not find entity %s" s1)
         | _ -> failwith (spf "multiple entities with %s as a name" s1));
  whitelist
  |> (*|> Console.progress ~show:true (fun k -> *)
  List.iter (fun (n1, n2) -> (*k (); *)
                             G.remove_edge (n1, n2) Use g)
